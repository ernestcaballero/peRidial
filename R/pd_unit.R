
#' Create a pd_unit object
#'
#' Low-level constructor for the unit-level reporting object. It wraps a PD
#' unit's patients, catheters and peritonitis episodes, both as flat tibbles
#' and as the nested \code{pd_patient} object list they were derived from;
#' together with the reporting period and the headline counts the ISPD
#' indicators are built from.
#'
#' This constructor does no derivation: every count is taken as supplied.
#' \code{\link{pd_unit}()} is the user-facing helper that reads the raw files,
#' builds the object graph, derives these numbers and validates the result.
#'
#' @param unit_id Character. Name of the PD unit (e.g. "XYZ PD Unit").
#' @param t0 Date. Start of the reporting period. Usually 1st day of January of the reporting year.
#' @param t1 Date. End of the reporting period. Usually 31st day of December of the reporting year.
#' @param n_new Integer. Number of incident ("new") patients in the reporting
#'   period, i.e. the count of \code{patient_list} entries whose \code{new_patient_flag} is \code{TRUE}.
#' @param n_patients Integer. Size of the reporting cohort: every patient who
#'   was on PD at any point during \code{[t0, t1]} (prevalent at \code{t0}
#'   plus incident within the period), including those who died or left PD
#'   part-way through. Must equal both \code{nrow(patients)} and
#'   \code{length(patient_list)}.
#' @param tpyar Numeric. Total patient-years at risk across the cohort: the
#'   time patients actively spent on PD within \code{[t0, t1]}, censored at
#'   each patient's \code{tau}, expressed in years. This is the denominator
#'   (PY) of the peritonitis rate. Derived by \code{\link{total_patient_years}()}.
#' @param rate_benchmark Numeric. The ISPD peritonitis-rate benchmark for
#'   this unit, in episodes per patient-year: the headline rate is judged
#'   "MET" when it is at or below this value. Carried on the object so
#'   \code{summary.pd_unit()} and \code{plot.pd_unit()} judge and plot
#'   against the same threshold by default. Defaults to 0.40, the ISPD
#'   standard.
#' @param patients Tibble. One row per patient, flattened from
#'   \code{patient_list}.
#' @param catheters Tibble. One row per catheter, flattened from the
#'   \code{pd_catheter} objects nested inside \code{patient_list}.
#' @param infections Tibble. One row per peritonitis episode, flattened from
#'   the \code{pd_infection} objects nested inside each catheter, and carrying
#'   the \code{catheter_id} of the catheter that owns it.
#' @param patient_list List of \code{pd_patient} objects: the nested object
#'   graph the three tibbles above are views of.
#'
#' @return An object of class \code{pd_unit}.
#' @seealso \code{\link{pd_unit}()} to build one from raw data files.
#' @export
#'
#' @examples
#' # An empty unit for a period with no patients.
#' new_pd_unit(
#'   unit_id = "Wellington PD Unit",
#'   t0 = as.Date("2025-01-01"),
#'   t1 = as.Date("2025-12-31"),
#'   n_new = 0L,
#'   n_patients = 0L,
#'   tpyar = 0
#' )
new_pd_unit <- function(unit_id = NA_character_,
                        t0 = as.Date(NA),        # start date
                        t1 = as.Date(NA),        # end date
                        n_new = NA_integer_,
                        n_patients = NA_integer_,
                        tpyar = NA_real_,        # total patient-years-at-risk
                        rate_benchmark = 0.40,   # ISPD peritonitis-rate benchmark
                        patients = tibble::tibble(),
                        catheters = tibble::tibble(),
                        infections = tibble::tibble(),
                        patient_list = list()) {

  stopifnot(length(unit_id) == 1, is.character(unit_id))
  stopifnot(inherits(t0, "Date"), length(t0) == 1)
  stopifnot(inherits(t1, "Date"), length(t1) == 1)
  stopifnot(length(n_new) == 1, is.na(n_new) || is.numeric(n_new))
  stopifnot(length(n_patients) == 1, is.na(n_patients) || is.numeric(n_patients))
  stopifnot(length(tpyar) == 1, is.na(tpyar) || is.numeric(tpyar))
  stopifnot(length(rate_benchmark) == 1, is.numeric(rate_benchmark))
  stopifnot(is.data.frame(patients), is.data.frame(catheters),
            is.data.frame(infections))
  stopifnot(is.list(patient_list))

  structure(
    list(
      unit_id = unit_id,
      t0 = t0,
      t1 = t1,
      n_new = n_new,
      n_patients = n_patients,
      tpyar = tpyar,
      rate_benchmark = rate_benchmark,
      patients = patients,
      catheters = catheters,
      infections = infections,
      patient_list = patient_list
    ),
    class = "pd_unit"
  )
}



validate_pd_unit <- function(x) {
  stopifnot(inherits(x, "pd_unit"))

  # checks if dates are missing
  if (is.na(x$t0) || is.na(x$t1)) {
    stop("Both t0 and t1 must be supplied.")
  }
  # checks if start date is later than end date
  if (x$t0 > x$t1) {
    stop("t0 must be on or before t1.")
  }

  # checks n_new and n_patients are missing
  if (is.na(x$n_new) || is.na(x$n_patients)) {
    stop("Both n_new and n_patients must be supplied.")
  }
  if (x$n_new < 0 || x$n_patients < 0) {
    stop("n_new and n_patients cannot be negative.")
  }
  # incident patients are a subset of the cohort, so this can't exceed it
  if (x$n_new > x$n_patients) {
    stop("n_new cannot exceed n_patients.")
  }
  # checks if total_patient_yrs_at_risk is missing
  if (is.na(x$tpyar)) {
    stop("Total patient-years-at-risk (tpyar) is missing.")
  }
  # checks if tpyar is less than 0
  if (x$tpyar < 0) {
    stop("Total patient-years-at-risk cannot be negative.")
  }
  # No patient can contribute more than the full period, so the cohort can't
  # accrue more patient-years than n_patients * the period's own length
  period_years <- as.numeric(x$t1 - x$t0 + 1) / 365.25
  if (x$tpyar > x$n_patients * period_years + 1e-8) {
    stop("Total patient-years-at-risk (", round(x$tpyar, 3), ") exceeds the ",
         "maximum possible for ", x$n_patients, " patients over a reporting ",
         "period of ", round(period_years, 3), " years.")
  }

  # checks rate_benchmark is present and sane
  if (is.na(x$rate_benchmark)) {
    stop("rate_benchmark is missing.")
  }
  if (x$rate_benchmark < 0) {
    stop("rate_benchmark cannot be negative.")
  }

  # `patients` and `patient_list` are two views of the same cohort: must match (nrow(patients) = n_patients)
  # We need to settle that they agree before checking anything that references them.
  if (nrow(x$patients) != x$n_patients) {
    stop("nrow(patients) (", nrow(x$patients), ") does not match n_patients (",
         x$n_patients, ").")
  }
  if (nrow(x$patients) > 0) {
    require_cols(x$patients, "patient_id", "patients", kind = "table")
    if (anyDuplicated(x$patients$patient_id) > 0) {
      stop("Duplicate patient_id(s) in `patients`: ",
           paste(unique(x$patients$patient_id[duplicated(x$patients$patient_id)]),
                 collapse = ", "), ".")
    }
  }
  # checks patient_list length matches n_patients
  if (length(x$patient_list) != x$n_patients) {
    stop("Length of `patient_list` (", length(x$patient_list), ") does not ",
         "match n_patients (", x$n_patients, ").")
  }
  # checks if patient_list is a pd_patient object
  if (length(x$patient_list) > 0) {
    for (i in seq_along(x$patient_list)) {
      p <- x$patient_list[[i]]
      if (!inherits(p, "pd_patient")) {
        stop("patient_list[[", i, "]] is not a pd_patient object.")
      }
      # every patient must be scoped to the unit's reporting window
      if (!identical(p$t0, x$t0) || !identical(p$t1, x$t1)) {
        stop("patient_list[[", i, "]] has a reporting window that differs from this unit's [t0, t1].")
      }
    }

    # the tibble is a view of the patient_list, so the two must agree
    obj_ids <- vapply(x$patient_list, function(p) p$patient_id, character(1))
    if (nrow(x$patients) > 0 && !setequal(obj_ids, x$patients$patient_id)) {
      stop("The patient_id values in `patients` do not match those in `patient_list`.")
    }

    # n_new is defined as the count of incident patients in patient_list
    expected_new <- sum(vapply(x$patient_list,
                               function(p) isTRUE(p$new_patient_flag),
                               logical(1)))
    if (x$n_new != expected_new) {
      stop("n_new (", x$n_new, ") does not match the number of patients in ",
           "`patient_list` flagged as incident (", expected_new, ").")
    }
  }

  # checks catheters reference a valid patient_id
  if (nrow(x$catheters) > 0) {
    require_cols(x$catheters, c("patient_id", "catheter_id"), "catheters", kind = "table")
    if (!all(x$catheters$patient_id %in% x$patients$patient_id)) {
      stop("Some catheter records reference a patient_id not present in `patients`.")
    }
    # catheter_id is the key infections are matched on, so it must be unique
    # across the whole unit, not just within a patient
    if (anyDuplicated(x$catheters$catheter_id) > 0) {
      stop("Duplicate catheter_id(s) in `catheters`: ",
           paste(unique(x$catheters$catheter_id[duplicated(x$catheters$catheter_id)]),
                 collapse = ", "), ".")
    }
  }
  # checks infections reference a valid catheter_id
  if (nrow(x$infections) > 0) {
    require_cols(x$infections, c("patient_id", "catheter_id"), "infections", kind = "table")
    if (!all(x$infections$catheter_id %in% x$catheters$catheter_id)) {
      stop("Some infection records reference a catheter_id not present in `catheters`.")
    }
    if (!all(x$infections$patient_id %in% x$patients$patient_id)) {
      stop("Some infection records reference a patient_id not present in `patients`.")
    }
  }

  x
}




#' Build a pd_unit object from raw data files
#'
#' Reads a unit's raw dialysis (A3) file and peritonitis-episode (PE) file and
#' builds the full nested object graph for a reporting period:
#' peritonitis episodes owned by the catheter that was active when they occurred,
#' catheters owned by their patient, and every patient who was on PD at any
#' point in \code{[t0, t1]} collected into one \code{pd_unit}.
#'
#' The returned object carries both the nested \code{patient_list} and three
#' flat tibbles (\code{patients}, \code{catheters}, \code{infections})
#' derived from it, plus the headline numbers the ISPD indicators need:
#' \code{n_new}, \code{n_patients} and \code{tpyar}.
#'
#' Data-quality problems (for example an episode that cannot be matched to an
#' active catheter, or a patient left with no valid catheter) are collected
#' across both files and raised together as a single error. Each one needs a
#' correction in the source file: fix them and run \code{pd_unit()} again until
#' it builds without error.
#'
#' @param unit_data_path Character. Path to the raw unit (A3) Excel file
#'   containing both patient and catheter data.
#' @param infection_data_path Character. Path to the raw infection/peritonitis
#'   episode (PE) Excel file.
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param unit_id Character. Identifier for the unit, e.g.
#'   \code{"Auckland PD Unit"}. Defaults to \code{NA_character_}.
#' @param rate_benchmark Numeric. The ISPD peritonitis-rate benchmark for
#'   this unit, in episodes per patient-year, used by \code{summary.pd_unit()}
#'   and \code{plot.pd_unit()} to judge/plot the headline rate against.
#'   Defaults to 0.40, the ISPD standard.
#' @param censor_on_last_stop Logical. Treat a patient whose every catheter
#'   has closed, with none reopened, as having left PD on the last
#'   \code{pd_stop_date}. Defaults to \code{TRUE}. Set \code{FALSE} if your
#'   unit records genuine breaks from PD, since a break and a permanent exit
#'   look identical in the raw data.
#'
#' @return A validated \code{pd_unit} object.
#' @seealso \code{\link{new_pd_unit}()} for the underlying constructor.
#' @export
#'
#' @examples
#' unit <- pd_unit(
#'   unit_data_path = system.file("extdata", "a3_2025.xlsx",
#'                                package = "peridial", mustWork = TRUE),
#'   infection_data_path = system.file("extdata", "pe_2025.xlsx",
#'                                     package = "peridial", mustWork = TRUE),
#'   t0 = as.Date("2025-01-01"),
#'   t1 = as.Date("2025-12-31"),
#'   unit_id = "Wellington PD Unit"
#' )
#' unit
pd_unit <- function(unit_data_path,
                    infection_data_path,
                    t0,
                    t1,
                    unit_id = NA_character_,
                    rate_benchmark = 0.40,
                    censor_on_last_stop = TRUE) {

  stopifnot(inherits(t0, "Date"), length(t0) == 1, !is.na(t0))
  stopifnot(inherits(t1, "Date"), length(t1) == 1, !is.na(t1))
  if (t0 > t1) {
    stop("t0 must be on or before t1.")
  }

  log <- new_issue_log()

  # Read file and standardise
  raw_a3 <- standardise_names(readxl::read_excel(unit_data_path))
  raw_pe_file <- standardise_names(readxl::read_excel(infection_data_path))

  # map differently-named columns onto the expected names by keyword; logged
  raw_a3 <- map_columns(raw_a3, a3_spec, "unit (A3)", log)
  raw_pe_file <- map_columns(raw_pe_file, pe_spec, "infection (PE)", log)

  a3_required_cells <- c("patient_id", "date_of_birth", "insertion_date", "pd_start_date")
  pe_required_cells <- c("patient_id", "date_of_infection", "organism", "last_dose_antibiotic")

  require_cols(raw_a3, a3_required_cells, "unit (A3)")
  require_cols(raw_pe_file, pe_required_cells, "infection (PE)")

  a3_optional <- c("date_of_birth", "gender", "ethnicity",
                   "primary_kidney_disease", "height", "weight",
                   "cigarette_smoking_status", "diabetes_type",
                   "dialysis_modality_change", "modality_change_reason",
                   "date_modality_change", "date_of_death", "cause_of_death",
                   "transplant_date", "dialysis_type",
                   "procedure_type", "pd_stop_date", "removal_reason")
  raw_a3 <- ensure_cols(raw_a3, a3_optional)

  pe_optional <- c("last_dose_antibiotic", "overnight_hospitalisation", "days_hospitalised",
                   "catheter_removed", "catheter_removed_date", "interim_hd",
                   "permanent_hd", "first_dialysis_date", "last_dialysis_date")
  raw_pe_file <- ensure_cols(raw_pe_file, pe_optional)

  # a blank required cell is logged and its dropped here
  raw_a3 <- check_required_cells(raw_a3, a3_required_cells, "unit (A3)", log)
  raw_pe_file <- check_required_cells(raw_pe_file, pe_required_cells, "infection (PE)", log)

  # tidy the A3 form into patients, catheters and modality changes
  raw_a3 <- raw_a3 |>
    dplyr::mutate(
      patient_id = trimws(as.character(patient_id)),
      date_of_birth = as_date_safe(date_of_birth, "date_of_birth"),
      date_of_death = as_date_safe(date_of_death, "date_of_death"),
      date_modality_change = as_date_safe(date_modality_change,"date_modality_change"),
      insertion_date = as_date_safe(insertion_date, "insertion_date"),
      pd_start_date = as_date_safe(pd_start_date, "pd_start_date"),
      pd_stop_date = as_date_safe(pd_stop_date, "pd_stop_date")
    )

  raw_patients <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "date_of_birth", "gender", "ethnicity",
      "primary_kidney_disease", "height", "weight",
      "cigarette_smoking_status", "diabetes_type", "date_of_death",
      "cause_of_death", "dialysis_type", "transplant_date"))) |>
    dplyr::distinct(patient_id, .keep_all = TRUE)

  raw_catheters <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "insertion_date", "procedure_type", "pd_start_date",
      "pd_stop_date", "removal_reason"))) |>
    dplyr::filter(!is.na(patient_id)) |>
    dplyr::mutate(catheter_id = create_catheter_id(patient_id, insertion_date))

  raw_modality <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "dialysis_modality_change", "modality_change_reason", "date_modality_change")))

  # tidy the PE form into episodes
  raw_pe_episodes <- raw_pe_file |>
    dplyr::mutate(
      patient_id = trimws(as.character(patient_id)),
      date_of_infection = as_date_safe(date_of_infection, "date_of_infection"),
      last_dose_antibiotic = as_date_safe(last_dose_antibiotic, "last_dose_antibiotic"),
      catheter_removed_date = as_date_safe(catheter_removed_date, "catheter_removed_date"),
      first_dialysis_date = as_date_safe(first_dialysis_date, "first_dialysis_date"),
      last_dialysis_date = as_date_safe(last_dialysis_date, "last_dialysis_date"),
      catheter_removed = as_logical_safe(catheter_removed), # normalise the yes/no flags before case_when() combines them
      permanent_hd = as_logical_safe(permanent_hd),
      interim_hd = as_logical_safe(interim_hd),
      overnight_hospitalisation = as_logical_safe(overnight_hospitalisation)
    ) |>
    dplyr::mutate(
      # each element must itself be a list: new_pd_infection() and get_episode_type() both require is.list(organism_list)
      organism_list = lapply(strsplit(as.character(organism), ",\\s*"),
                             function(z) as.list(trimws(z))),
      # Outcome priority is catheter removal > permanent HD > temporary HD > hospitalisation; the most severe flag set wins.
      outcome = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE)          ~ "catheter removed",
        dplyr::coalesce(permanent_hd, FALSE)              ~ "permanent transfer to HD",
        dplyr::coalesce(interim_hd, FALSE)                ~ "temporary transfer to HD",
        dplyr::coalesce(overnight_hospitalisation, FALSE) ~ "hospitalisation",
        TRUE ~ NA_character_
      ),
      # outcome/outcome_date can be nullable: resolved episode falls through to NA, which validate_pd_infection() reads as implied good recovery
      outcome_date = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE) ~ catheter_removed_date,
        dplyr::coalesce(permanent_hd, FALSE) |
          dplyr::coalesce(interim_hd, FALSE)     ~ first_dialysis_date,
        TRUE ~ as.Date(NA)
      )
    ) |>
    dplyr::arrange(patient_id, date_of_infection)

  # Derive tau per patient, then censor the catheter windows
  pids <- sort(unique(raw_catheters$patient_id))
  taus <- list()
  for (pid in pids) {
    taus[[pid]] <- derive_patient_tau(
      demo = raw_patients[raw_patients$patient_id == pid, , drop = FALSE],
      mod  = raw_modality[raw_modality$patient_id == pid, , drop = FALSE],
      cath = raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE],
      censor_on_last_stop = censor_on_last_stop
    )
    tau <- taus[[pid]]$date

    # tau is the authority on when PD ended: close any catheter left open past it, and pull back any that claims to have ran beyond it
    if (!is.na(tau)) {
      rows <- which(raw_catheters$patient_id == pid)
      open <- rows[is.na(raw_catheters$pd_stop_date[rows])]
      if (length(open) > 0) {
        raw_catheters$pd_stop_date[open] <- tau
      }
      late <- rows[!is.na(raw_catheters$pd_stop_date[rows]) &
                     raw_catheters$pd_stop_date[rows] > tau]
      if (length(late) > 0) {
        log$add("Patient ", pid, ": catheter(s) ",
                paste(raw_catheters$catheter_id[late], collapse = ", "),
                " have a pd_stop_date after this patient's ",
                taus[[pid]]$reason, " on ", format(tau),
                "; clipped to that date.")
        raw_catheters$pd_stop_date[late] <- tau
      }
    }

    # a PD-to-HD transfer with no reason recorded is a gap in the source data
    if (identical(taus[[pid]]$reason, "permanent transfer to HD")) {
      detail <- taus[[pid]]$detail
      if (is.na(detail) || !nzchar(detail)) {
        log$add("Patient ", pid, " has an 'Any PD to HD' modality change on ",
                format(taus[[pid]]$date),
                " with no modality_change_reason.")
      }
    }

    # a 'transplant' modality change with no date is a gap: the date can only come from transplant_date
    if (isTRUE(taus[[pid]]$transplant_gap)) {
      log$add("Patient ", pid, " has a 'transplant' modality change recorded ",
              "but no transplant_date supplied (and no catheter ",
              "removal_reason mentioning transplant); this patient's true ",
              "censoring date is unknown and they are being treated as if ",
              "still active on PD.")
    }

    # for 'Any PD to HD': the modality change was recorded the row is missing date_modality_change
    if (isTRUE(taus[[pid]]$hd_transfer_gap)) {
      log$add("Patient ", pid, " has an 'Any PD to HD' modality change ",
              "recorded but no date_modality_change supplied for it; this ",
              "patient's true censoring date is unknown and they are being ",
              "treated as if still active on PD.")
    }
  }

  # Build the object graph, now that tau has clipped the catheter windows:
  # episodes are matched to the catheter active on their date, catheters own
  # their episodes, and patients own their catheters
  infections_by_catheter <- build_infections_by_catheter(raw_pe_episodes,
                                                         raw_catheters, log)
  built <- build_patient_list(pids, taus, raw_catheters, raw_patients,
                              infections_by_catheter, t0, t1, log)
  patient_list <- built$patient_list
  transfer_details <- built$transfer_details

  # Flatten the object graph into the unit's three tibbles
  patients_tbl <- patients_to_tibble(patient_list, t0, t1, transfer_details)
  catheters_tbl <- catheters_to_tibble(patient_list, t0, t1)
  infections_tbl <- infections_to_tibble(patient_list, t0, t1)

  # Unit-level numbers, cohort size: everyone on PD at any point in [t0, t1].
  n_patients <- length(patient_list)

  # Incident ("new") patients: earliest PD start inside [t0, t1]. A patient with
  # no usable start dates has flag NA and is not counted.
  n_new <- sum(vapply(patient_list, function(p) isTRUE(p$new_patient_flag),
                      logical(1)))

  # PY: total patient-years at risk, censored to [t0, t1] and each patient's tau
  tpyar <- total_patient_years(patient_list, t0, t1)

  # any data-quality issue is an error: the source files need correcting
  report_issues(log)

  x <- new_pd_unit(
    unit_id        = unit_id,
    t0             = t0,
    t1             = t1,
    n_new          = as.integer(n_new),
    n_patients     = as.integer(n_patients),
    tpyar          = tpyar,
    rate_benchmark = rate_benchmark,
    patients       = patients_tbl,
    catheters    = catheters_tbl,
    infections   = infections_tbl,
    patient_list = patient_list
  )

  validate_pd_unit(x)
}




#' Print a pd_unit object
#'
#' @param x A \code{pd_unit} object.
#' @param ... Ignored.
#'
#' @return \code{x}, invisibly.
#' @export
#'
print.pd_unit <- function(x, ...) {
  cat("<pd_unit>", if (is.na(x$unit_id)) "(Unnamed unit)" else x$unit_id, "\n")
  cat("  Reporting period      : ", format(x$t0), " to ", format(x$t1), "\n", sep = "")
  cat("  Patients              : ", x$n_patients,
      " (", x$n_new, " incident)\n", sep = "")
  cat("  Catheters             : ", nrow(x$catheters), "\n", sep = "")
  cat("  Peritonitis episodes  : ", nrow(x$infections), "\n", sep = "")
  cat("  Total patient-years   : ", format(round(x$tpyar, 2), nsmall = 2), "\n", sep = "")
  invisible(x)
}




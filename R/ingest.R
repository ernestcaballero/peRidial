#' Generate catheter_id values from patient_id and insertion_date
#'
#' A patient can have more than one PD catheter over time, so a catheter is
#' identified by which patient it belongs to and when it was
#' inserted relative to that patient's other catheters. This builds
#' \code{catheter_id} as \code{patient_id} plus a two-digit sequence number
#' ordered by \code{insertion_date} within each patient (e.g. a patient
#' \code{"XYZ1234"}'s first catheter becomes \code{"XYZ1234_01"}, their next
#' one (if any) \code{"XYZ1234_02"}, and so on). See the \code{catheter_id}
#' documentation in \code{pd_catheter()}.
#'
#' @param patient_id Character vector. Patient identifier for each row (NHI).
#' @param insertion_date Date vector, the same length as \code{patient_id}.
#'   Date each row's catheter was inserted; used to order multiple catheters
#'   within the same patient.
#'
#' @return Character vector, same length and order as the inputs, giving each
#'   row's generated \code{catheter_id}.
#' @noRd
#'
create_catheter_id <- function(patient_id, insertion_date) {
  stopifnot(length(patient_id) == length(insertion_date))
  stopifnot(inherits(insertion_date, "Date"))

  catheter_id <- character(length(patient_id))

  for (pid in unique(patient_id)) {
    rows <- which(patient_id == pid)
    ord <- rows[order(insertion_date[rows])]
    catheter_id[ord] <- paste0(pid, "_", sprintf("%02d", seq_along(ord)))
  }

  catheter_id
}




#' Build pd_unit object from raw data files
#'
#' Reads a unit's raw dialysis data file and infection data file
#' and builds patient, catheter, and peritonitis-episode data
#' for a reporting period.
#'
#' @param unit_data_path Character. Path to the raw unit Excel file
#'   containing both patient and catheter data.
#' @param infection_data_path Character. Path to the raw infection/peritonitis
#'   episode Excel file.
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param unit_id Character. Optional identifier for the unit. Defaults to
#'   \code{NA_character_} if not supplied.
#'
#' @return A \code{pd_unit} object containing the patient, catheter, and
#'   peritonitis episode data for the unit over the reporting period
#'   \code{t0} to \code{t1}.
#' @export
#'

pd_unit <- function(unit_data_path,
                    infection_data_path,
                    t0,
                    t1,
                    unit_id = NA_character_) {

  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))

  # Read raw files, change date format %d-%m-%Y
  raw_a3 <- readxl::read_excel(unit_data_path)

  # standardise column names here (can use function)

  raw_patients <- raw_a3 |>
    dplyr::select(patient_id, date_of_birth, gender, ethnicity,
                  primary_kidney_disease_code, height, weight,
                  cigarette_smoking_status, diabetes_type,
                  modality_change_reason_code,
                  date_of_death, cause_of_death,
                  current_centre_name, date_transfer_current,
                  last_centre_name, date_transfer_last,
                  dialysis_type_code, dry_weight_at_last_dx_kg) |>
    dplyr::mutate(
      date_of_birth = as.Date(date_of_birth, format = "%Y-%m-%d"),
      date_of_death = as.Date(date_of_death, format = "%Y-%m-%d"),
      date_transfer_current = as.Date(date_transfer_current, format = "%Y-%m-%d"),
      date_transfer_last = as.Date(date_transfer_last, format = "%Y-%m-%d")) |>
    dplyr::distinct(patient_id, .keep_all = TRUE)

  raw_catheters <- raw_a3 |>
    dplyr::select(patient_id, catheter_id, insertion_date, procedure_type,
                  pd_start_date, pd_stop_date, removal_reason,
                  peritonitis_date_first_episode,
                  peritonitis_episodes_count) |>
    dplyr::mutate(
      insertion_date = as.Date(insertion_date, format = "%Y-%m-%d"),
      pd_start_date = as.Date(pd_start_date, format = "%Y-%m-%d"),
      pd_stop_date = as.Date(pd_stop_date, format = "%Y-%m-%d"),
      peritonitis_date_first_episode = as.Date(peritonitis_date_first_episode, format = "%Y-%m-%d")
    )

  raw_pe <- readxl::read_excel(infection_data_path) |>
  dplyr::select(patient_id, date_of_infection,
                relapse_recurrence_code, organism, last_dose_antibiotic,
                overnight_hospitalisation, days_hospitalised,
                catheter_removed, catheter_removed_date,
                interim_hd, permanent_hd, first_dialysis_date, last_dialysis_date) |>
  dplyr::mutate(
    date_of_infection = as.Date(date_of_infection, format = "%Y-%m-%d"),
    last_dose_antibiotic = as.Date(last_dose_antibiotic, format = "%Y-%m-%d"),
    catheter_removed_date = as.Date(catheter_removed_date, format = "%Y-%m-%d"),
    first_dialysis_date = as.Date(first_dialysis_date, format = "%Y-%m-%d"),
    last_dialysis_date = as.Date(last_dialysis_date, format = "%Y-%m-%d")
  )

  # Derive organism_list and outcome for each episode
  # Build a label per row by checking if catheter removed, permanent HD, interim HD, hospitalisation or none/resolved
  raw_pe_episodes <- raw_pe |>
    dplyr::mutate(
      organism_list = lapply(strsplit(organism, ",\\s*"), trimws),
      outcome = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE)           ~ "catheter removed",
        dplyr::coalesce(permanent_hd, FALSE)               ~ "permanent transfer to HD",
        dplyr::coalesce(interim_hd, FALSE)                 ~ "temporary transfer to HD",
        dplyr::coalesce(overnight_hospitalisation, FALSE)  ~ "hospitalisation",
        TRUE ~ NA_character_
      ),
      # Populates outcome date if outcome is specified
      outcome_date = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE) ~ catheter_removed_date,
        dplyr::coalesce(permanent_hd, FALSE) | dplyr::coalesce(interim_hd, FALSE) ~ first_dialysis_date,
        TRUE ~ as.Date(NA)
      )
    ) |>
    # Sorts the table by patient first, then infection date/s within each patient
    dplyr::arrange(patient_id, date_of_infection)


  # Build infection objects by patients
  build_patient_infections <- function(df) {
    infections <- vector("list", nrow(df))
    prior <- NULL
    for (i in seq_len(nrow(df))) {
      infections[[i]] <- pd_infection(
        patient_id = df$patient_id[i],
        infection_date = df$date_of_infection[i],
        organism_list = df$organism_list[[i]],
        last_dose_antibiotic = df$last_dose_antibiotic[i],
        outcome = df$outcome[i],
        outcome_date = df$outcome_date[i],
        prior_episode = prior
      )
      prior <- infections[[i]]
    }
    infections
  }

  infections_by_patient <- raw_pe_episodes |>
    split(raw_pe_episodes$patient_id) |>
    purrr::map(build_patient_infections)

  # Match each pd_infection to the PD catheter that was active on its infection_date.
  match_active_catheter_id <- function(pid, infection_date) {
    cath <- raw_catheters[raw_catheters$patient_id == pid, ]
    active <- cath$pd_start_date <= infection_date &
      (is.na(cath$pd_stop_date) | infection_date <= cath$pd_stop_date)
    candidates <- cath$catheter_id[active]

    if (length(candidates) == 0) {
      warning("No active PD catheter found for patient ", pid,
              " on infection_date ", infection_date,
              "; this infection will not be attached to a catheter.",
              call. = FALSE)
      return(NA_character_)
    }
    if (length(candidates) > 1) {
      detail <- paste0(
        candidates, " (insertion_date=", cath$insertion_date[active],
        ", pd_start_date=", cath$pd_start_date[active],
        ", pd_stop_date=", cath$pd_stop_date[active], ")"
      )
      stop("Multiple active PD catheters found for patient ", pid,
           " on infection_date ", infection_date, ":\n",
           paste("  -", detail, collapse = "\n"),
           "\nPlease check and correct these catheters' insertion_date, ",
           "pd_start_date and/or pd_stop_date in the source data so each ",
           "patient's catheter windows don't overlap, then re-run.")
    }
    candidates[1]
  }

  all_infections <- unlist(infections_by_patient, recursive = FALSE, use.names = FALSE)

  # Flatten to one list of pd_infection objects, match each to its active catheter_id,
  # then regroup by that id
  infections_by_catheter <- if (length(all_infections) > 0) {
    matched_catheter_id <- vapply(all_infections, function(inf) {
      match_active_catheter_id(inf$patient_id, inf$infection_date)
    }, character(1))

    matched <- !is.na(matched_catheter_id)
    split(all_infections[matched], matched_catheter_id[matched])
  } else {
    list()
  }


  # Build catheter objects per patient, attaching each catheter's own
  # infections from infections_by_catheter
  build_patient_catheters <- function(pid) {
    df <- raw_catheters[raw_catheters$patient_id == pid, ]
    purrr::pmap(df, function(patient_id, catheter_id, insertion_date,
                             procedure_type, pd_start_date, pd_stop_date,
                             removal_reason, peritonitis_date_first_episode,
                             peritonitis_episodes_count) {
      cath_infections <- infections_by_catheter[[catheter_id]]
      if (is.null(cath_infections)) cath_infections <- list()

      pd_catheter(
        patient_id = patient_id,
        catheter_id = catheter_id,
        insertion_date = insertion_date,
        procedure_type = procedure_type,
        pd_start_date = pd_start_date,
        pd_stop_date = pd_stop_date,
        removal_reason = removal_reason,
        infections = cath_infections,
        # included t0/t1 come from pd_unit() so n_peritonitis_episodes/peritonitis_flag
        # are in scope of the survey period
        t0 = t0,
        t1 = t1
      )
    })
  }


  # Build patient objects, nesting catheters
  patient_list <- purrr::map(raw_patients$patient_id, function(pid) {
    row <- raw_patients[raw_patients$patient_id == pid, ]

    pd_patient(
      patient_id = row$patient_id,
      catheters = build_patient_catheters(pid),
      t0 = t0,
      t1 = t1
      # demographics are parked until the demographics list is added back to
      # pd_patient() -- the raw columns are already selected in raw_patients:
      #   date_of_birth, gender, ethnicity, primary_kidney_disease_code,
      #   height, weight, cigarette_smoking_status, diabetes_type,
      #   date_of_death, cause_of_death
      #
      # TODO: transfer_reason/transfer_date (tau -- death, transplant, or
      # permanent HD transfer) also need mapping from the A3 columns
      # (date_of_death/cause_of_death/modality_change_reason_code) so that
      # patient-level censoring is applied to exposure days.
    )
  })
  names(patient_list) <- raw_patients$patient_id


  # Unit-level numbers

  # Total PD cohort
  n_patients <- nrow(raw_patients)

  # Count incident ("new") patients -- those whose earliest catheter start date
  # falls inside [t0, t1]. Each pd_patient already derived this as
  # new_patient_flag via is_incident_patient(), so summing the flags keeps a
  # single definition of "incident" rather than recomputing it from the raw
  # table. A patient with no usable start dates has flag NA and is not counted.
  n_new <- sum(
    vapply(patient_list, function(p) isTRUE(p$new_patient_flag), logical(1))
  )

  # Total patient-years at risk, censored to [t0, t1] and to each patient's
  # tau. Computed from patient_list, not raw_catheters, because tau lives on
  # the pd_patient objects -- see total_patient_years() in indicators.R.
  tpyar <- total_patient_years(patient_list, t0, t1)


  x <- new_pd_unit(
    unit_id = unit_id,
    t0 = t0,
    t1 = t1,
    n_new = as.integer(n_new),
    n_patients = as.integer(n_patients),
    tpyar = tpyar,
    patients = raw_patients,
    catheters = raw_catheters,
    infections = raw_pe_episodes,
    patient_list = patient_list
  )

  validate_pd_unit(x)
}


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
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param n_new Integer. Number of incident ("new") patients in the reporting
#'   period, i.e. the count of \code{patient_list} entries whose
#'   \code{new_patient_flag} is \code{TRUE}.
#' @param n_patients Integer. Size of the reporting cohort: every patient who
#'   was on PD at any point during \code{[t0, t1]} (prevalent at \code{t0}
#'   plus incident within the period), including those who died or left PD
#'   part-way through. Must equal both \code{nrow(patients)} and
#'   \code{length(patient_list)}.
#' @param tpyar Numeric. Total patient-years at risk across the cohort: the
#'   time patients actively spent on PD within \code{[t0, t1]}, censored at
#'   each patient's \code{tau}, expressed in years. This is the denominator
#'   (PY) of the peritonitis rate. Derived by
#'   \code{\link{total_patient_years}()}.
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
#'   unit_id = "Auckland PD Unit",
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

  # `patients` and `patient_list` are two views of the same cohort: must match (nrow(patients) = n_patients)
  # We need to settle that they agree before checking anything that references them.
  if (nrow(x$patients) != x$n_patients) {
    stop("nrow(patients) (", nrow(x$patients), ") does not match n_patients (",
         x$n_patients, ").")
  }
  if (nrow(x$patients) > 0) {
    require_unit_cols(x$patients, "patient_id", "patients")
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
    require_unit_cols(x$catheters, c("patient_id", "catheter_id"), "catheters")
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
    require_unit_cols(x$infections, c("patient_id", "catheter_id"), "infections")
    if (!all(x$infections$catheter_id %in% x$catheters$catheter_id)) {
      stop("Some infection records reference a catheter_id not present in `catheters`.")
    }
    if (!all(x$infections$patient_id %in% x$patients$patient_id)) {
      stop("Some infection records reference a patient_id not present in `patients`.")
    }
  }

  x
}


#' Check that a unit-level tibble carries the columns validation needs
#'
#' @param df A data frame.
#' @param cols Character vector of required column names.
#' @param what Character. Name of the field being checked, for the message.
#'
#' @return Invisibly \code{TRUE}; errors otherwise.
#' @noRd
#'
require_unit_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("`", what, "` is missing required column(s): ",
         paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
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



# HELPER FUNCTIONS FOR SUMMARY METHOD

#' Check that a unit-level tibble carries a usable column
#'
#' @param df A data frame.
#' @param col Character. Column name to look for.
#'
#' @return \code{TRUE} if \code{df} has at least one row and a column named
#'   \code{col}; \code{FALSE} otherwise.
#' @noRd
#'
has_col <- function(df, col) nrow(df) > 0 && col %in% names(df)


#' Peritonitis rate (Equation \eqref{eq:rate}) against its ISPD benchmark
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list with \code{rate}, \code{rate_num}, \code{rate_den},
#'   \code{rate_benchmark} and \code{rate_met}.
#' @noRd
#'
summarise_rate <- function(x) {
  rate_num <- if (has_col(x$infections, "counts_toward_rate")) {
    sum(x$infections$counts_toward_rate, na.rm = TRUE)
  } else {
    0
  }
  rate_den <- x$tpyar
  rate <- if (is.na(rate_den) || rate_den == 0) NA_real_ else rate_num / rate_den
  rate_benchmark <- 0.40
  list(rate = rate, rate_num = rate_num, rate_den = rate_den,
       rate_benchmark = rate_benchmark,
       rate_met = !is.na(rate) && rate <= rate_benchmark)
}


#' Peritonitis-free percentage (Equation \eqref{eq:pf}) against its ISPD benchmark
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list with \code{pf}, \code{pf_num}, \code{pf_den},
#'   \code{pf_benchmark} and \code{pf_met}.
#' @noRd
#'
summarise_pf <- function(x) {
  pf_num <- if (has_col(x$patients, "n_episodes")) {
    sum(x$patients$n_episodes == 0, na.rm = TRUE)
  } else {
    0
  }
  pf_den <- x$n_patients
  pf <- if (is.na(pf_den) || pf_den == 0) NA_real_ else pf_num / pf_den
  pf_benchmark <- 0.80
  list(pf = pf, pf_num = pf_num, pf_den = pf_den,
       pf_benchmark = pf_benchmark,
       pf_met = !is.na(pf) && pf > pf_benchmark)
}


#' Episode counts by \code{episode_type}
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A table, NA episode types relabelled \code{"uncategorised"}.
#' @noRd
#'
summarise_episode_types <- function(x) {
  if (has_col(x$infections, "episode_type")) {
    types <- x$infections$episode_type
    types[is.na(types)] <- "uncategorised"
    table(types)
  } else {
    table(character(0))
  }
}


#' Patient counts by how their PD ended
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A table by \code{transfer_reason} (\code{death},
#'   \code{transplant}, \code{permanent transfer to HD}, \code{pd stopped}),
#'   with \code{NA} (not yet censored) relabelled \code{"still active"}.
#' @noRd
#'
summarise_outcomes <- function(x) {
  if (has_col(x$patients, "transfer_reason")) {
    reasons <- x$patients$transfer_reason
    reasons[is.na(reasons)] <- "still active"
    table(reasons)
  } else {
    table(character(0))
  }
}


#' Incident / prevalent split of the cohort
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A named integer vector, \code{incident} and \code{prevalent}.
#' @noRd
#'
summarise_cohort <- function(x) {
  c(incident = x$n_new, prevalent = x$n_patients - x$n_new)
}


#' Median patient age at \code{t0}
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A single numeric (years), or \code{NA} if \code{date_of_birth}
#'   isn't available.
#' @noRd
#'
summarise_median_age <- function(x) {
  if (!has_col(x$patients, "date_of_birth")) {
    return(NA_real_)
  }
  ages <- as.numeric(difftime(x$t0, x$patients$date_of_birth, units = "days")) / 365.25
  if (all(is.na(ages))) NA_real_ else stats::median(ages, na.rm = TRUE)
}


#' Patient demographics, one table per field
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list of tables -- \code{gender}, \code{ethnicity},
#'   \code{primary_kidney_disease}, \code{diabetes_status},
#'   \code{smoking_status}, \code{dialysis_type} -- \code{NA} relabelled
#'   \code{"unknown"} in each.
#' @noRd
#'
summarise_demographics <- function(x) {
  demo_table <- function(col) {
    if (!has_col(x$patients, col)) {
      return(table(character(0)))
    }
    vals <- x$patients[[col]]
    vals[is.na(vals)] <- "unknown"
    table(vals)
  }
  list(
    gender = demo_table("gender"),
    ethnicity = demo_table("ethnicity"),
    primary_kidney_disease = demo_table("primary_kidney_disease"),
    diabetes_status = demo_table("diabetes_status"),
    smoking_status = demo_table("smoking_status"),
    dialysis_type = demo_table("dialysis_type")
  )
}


#' Quartiles (Q1, median, Q3) of a numeric vector
#'
#' A small formatting-friendly wrapper around \code{stats::quantile()}: always
#' returns a length-3 named vector, \code{NA} when there's nothing to
#' summarise rather than an error or a zero-length result.
#'
#' @param v Numeric vector. \code{NA}s are dropped before summarising.
#'
#' @return A named numeric vector, \code{Q1}/\code{Median}/\code{Q3}.
#' @noRd
#'
days_quartiles <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return(c(Q1 = NA_real_, Median = NA_real_, Q3 = NA_real_))
  }
  q <- stats::quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 7)
  stats::setNames(as.numeric(q), c("Q1", "Median", "Q3"))
}


#' Catheter summary: counts, procedure type, and two timing gaps
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list with \code{n_catheters}, \code{procedure_type} (a table,
#'   \code{NA} relabelled \code{"unknown"}), \code{insertion_to_start_days}
#'   (Q1/median/Q3 of insertion date to PD start date, in days) and
#'   \code{start_to_infection_days} (Q1/median/Q3 of each catheter's PD start
#'   date to the infection date of every episode recorded against it, in
#'   days).
#' @noRd
#'
summarise_catheters <- function(x) {
  n_catheters <- nrow(x$catheters)

  procedure_type <- if (has_col(x$catheters, "procedure_type")) {
    pt <- x$catheters$procedure_type
    pt[is.na(pt)] <- "unknown"
    table(pt)
  } else {
    table(character(0))
  }

  insertion_to_start_days <- if (has_col(x$catheters, "insertion_date") &&
                                 has_col(x$catheters, "pd_start_date")) {
    days_quartiles(as.numeric(x$catheters$pd_start_date - x$catheters$insertion_date))
  } else {
    days_quartiles(numeric(0))
  }

  # each infection is matched to the catheter that was active when it occurred, join on catheter_id
  start_to_infection_days <- if (has_col(x$infections, "infection_date") &&
                                 has_col(x$infections, "catheter_id") &&
                                 has_col(x$catheters, "pd_start_date") &&
                                 has_col(x$catheters, "catheter_id")) {
    starts <- x$catheters$pd_start_date[match(x$infections$catheter_id, x$catheters$catheter_id)]
    days_quartiles(as.numeric(x$infections$infection_date - starts))
  } else {
    days_quartiles(numeric(0))
  }

  list(n_catheters = n_catheters,
       procedure_type = procedure_type,
       insertion_to_start_days = insertion_to_start_days,
       start_to_infection_days = start_to_infection_days)
}


#' Infection detail: organisms cultured, PE outcomes, culture-negative rate
#' and peritonitis-related catheter removal
#'
#' \code{x$infections$organisms} is a comma-joined string per episode (an
#' episode can culture more than one organism), so this re-splits it to
#' tabulate at the organism level rather than the row level. An episode with
#' no organism recorded serialises to \code{""} or the literal
#' string \code{"NA"} (a list holding a missing value) -- neither is a real
#' organism name, so both are dropped before tabulating.
#'
#' @param x A \code{pd_unit} object.
#'
#' @return A list with \code{organisms} and \code{pe_outcomes} tables
#'   (\code{pe_outcomes}' \code{NA}, no complication flagged, is relabelled
#'   \code{"none"}); \code{culture_negative_n} and \code{culture_negative_pct}
#'   (episodes with no organism identified, out of all episodes); and
#'   \code{peritonitis_removal_n} and \code{peritonitis_removal_pct}
#'   (catheters whose \code{removal_reason} mentions peritonitis, out of all
#'   catheters).
#' @noRd
#'
summarise_infections <- function(x) {
  organisms <- if (has_col(x$infections, "organisms")) {
    raw <- x$infections$organisms
    raw <- raw[!is.na(raw)]
    tokens <- trimws(unlist(strsplit(raw, ",\\s*")))
    tokens <- tokens[nzchar(tokens) & tokens != "NA"]
    if (length(tokens) == 0) table(character(0)) else table(tokens)
  } else {
    table(character(0))
  }

  pe_outcomes <- if (has_col(x$infections, "outcome")) {
    out <- x$infections$outcome
    out[is.na(out)] <- "none"
    table(out)
  } else {
    table(character(0))
  }

  n_infections <- nrow(x$infections)
  culture_negative_n <- if (has_col(x$infections, "organisms")) {
    is_negative <- vapply(x$infections$organisms, function(v) {
      if (is.na(v)) return(TRUE)
      toks <- trimws(unlist(strsplit(v, ",\\s*")))
      toks <- toks[nzchar(toks) & toks != "NA"]
      length(toks) == 0 || all(tolower(toks) == "negative")
    }, logical(1))
    sum(is_negative)
  } else {
    0L
  }
  culture_negative_pct <- if (n_infections == 0) NA_real_ else culture_negative_n / n_infections

  n_catheters <- nrow(x$catheters)
  peritonitis_removal_n <- if (has_col(x$catheters, "removal_reason")) {
    sum(grepl("peritonitis", x$catheters$removal_reason, ignore.case = TRUE), na.rm = TRUE)
  } else {
    0L
  }
  peritonitis_removal_pct <- if (n_catheters == 0) NA_real_ else peritonitis_removal_n / n_catheters

  list(organisms = organisms, pe_outcomes = pe_outcomes,
       culture_negative_n = culture_negative_n,
       culture_negative_pct = culture_negative_pct,
       peritonitis_removal_n = peritonitis_removal_n,
       peritonitis_removal_pct = peritonitis_removal_pct)
}


#' Summarise a pd_unit object
#'
#' Reports the unit's headline peritonitis indicators (Equations
#' \eqref{eq:rate} and \eqref{eq:pf} against their ISPD benchmarks), a
#' breakdown of episodes by type, the cohort's outcomes and
#' incident/prevalent split, patient demographics, a catheter summary, and infection detail (organisms cultured and PE outcomes).
#'
#' @param object A \code{pd_unit} object.
#' @param ... Ignored.
#'
#' @return Invisibly, a list with components \code{rate}, \code{rate_num},
#'   \code{rate_den}, \code{rate_benchmark}, \code{rate_met}, \code{pf},
#'   \code{pf_num}, \code{pf_den}, \code{pf_benchmark}, \code{pf_met},
#'   \code{episode_types} (a table of episode counts by \code{episode_type}),
#'   \code{outcomes} (a table of patient counts by how their PD ended --
#'   \code{death}, \code{transplant}, \code{permanent transfer to HD},
#'   \code{pd stopped}, or \code{still active}), \code{cohort} (a named
#'   integer vector of \code{incident}/\code{prevalent} counts),
#'   \code{median_age_years} (this cohort's median patient age at \code{t0}),
#'   \code{demographics} (a list of tables -- \code{gender}, \code{ethnicity},
#'   \code{primary_kidney_disease}, \code{diabetes_status},
#'   \code{smoking_status}, \code{dialysis_type}), \code{catheters} (a list
#'   with \code{n_catheters}, \code{procedure_type}, and the
#'   \code{insertion_to_start_days} / \code{start_to_infection_days} timing
#'   gaps as Q1/median/Q3) and \code{infections} (a list with
#'   \code{organisms} and \code{pe_outcomes} tables, plus
#'   \code{culture_negative_n}/\code{_pct} and
#'   \code{peritonitis_removal_n}/\code{_pct}).
#' @export
#'
summary.pd_unit <- function(object, ...) {
  x <- object

  rate <- summarise_rate(x)
  pf <- summarise_pf(x)
  episode_types <- summarise_episode_types(x)
  outcomes <- summarise_outcomes(x)
  cohort <- summarise_cohort(x)
  median_age_years <- summarise_median_age(x)
  demographics <- summarise_demographics(x)
  catheters <- summarise_catheters(x)
  infections <- summarise_infections(x)

  fmt_demo <- function(tbl) {
    if (length(tbl) == 0) return("(no data)")
    paste(sprintf("%s %d", names(tbl), tbl), collapse = ", ")
  }

  fmt_quartiles <- function(q) {
    if (all(is.na(q))) return("NA")
    sprintf("%.1f / %.1f / %.1f", q[["Q1"]], q[["Median"]], q[["Q3"]])
  }

  fmt_n_pct <- function(n, pct) {
    if (is.na(pct)) sprintf("%d (NA)", n) else sprintf("%d (%.1f%%)", n, pct * 100)
  }

  cat("<summary.pd_unit>", if (is.na(x$unit_id)) "(Unnamed unit)" else x$unit_id, "\n")
  cat("  Reporting period : ", format(x$t0), " to ", format(x$t1), "\n\n", sep = "")

  cat("  Peritonitis rate : ",
      if (is.na(rate$rate)) "NA" else sprintf("%.3f", rate$rate),
      " episodes/patient-year\n", sep = "")
  cat("    numerator (countable episodes)      : ", rate$rate_num, "\n", sep = "")
  cat("    denominator (patient-years at risk) : ", sprintf("%.2f", rate$rate_den), "\n", sep = "")
  cat("    ISPD benchmark <= ", sprintf("%.2f", rate$rate_benchmark),
      "  [ ", if (rate$rate_met) "MET" else "NOT MET", " ]\n\n", sep = "")

  cat("  Peritonitis-free (PF) : ",
      if (is.na(pf$pf)) "NA" else sprintf("%.1f%%", pf$pf * 100), "\n", sep = "")
  cat("    numerator (patients with zero countable episodes)   : ", pf$pf_num, "\n", sep = "")
  cat("    denominator (total patients, N)                     : ", pf$pf_den, "\n", sep = "")
  cat("    ISPD benchmark >  ", sprintf("%.0f%%", pf$pf_benchmark * 100),
      "  [ ", if (pf$pf_met) "MET" else "NOT MET", " ]\n\n", sep = "")

  cat("  Peritonitis episodes by type:\n")
  if (length(episode_types) == 0) {
    cat("    (no episodes recorded)\n")
  } else {
    for (nm in names(episode_types)) {
      cat("    ", nm, " : ", episode_types[[nm]], "\n", sep = "")
    }
  }
  cat("\n")

  cat("  Cohort outcomes:\n")
  if (length(outcomes) == 0) {
    cat("    (no patients recorded)\n")
  } else {
    for (nm in names(outcomes)) {
      cat("    ", nm, " : ", outcomes[[nm]], "\n", sep = "")
    }
  }
  cat("\n")

  cat("  Patient demographics:\n")
  cat("    Median age      : ",
      if (is.na(median_age_years)) "unknown" else sprintf("%.0f (at t0)", median_age_years),
      "\n", sep = "")
  cat("    Gender          : ", fmt_demo(demographics$gender), "\n", sep = "")
  cat("    Ethnicity       : ", fmt_demo(demographics$ethnicity), "\n", sep = "")
  cat("    Kidney disease  : ", fmt_demo(demographics$primary_kidney_disease), "\n", sep = "")
  cat("    Diabetes        : ", fmt_demo(demographics$diabetes_status), "\n", sep = "")
  cat("    Smoking status  : ", fmt_demo(demographics$smoking_status), "\n", sep = "")
  cat("    Dialysis type   : ", fmt_demo(demographics$dialysis_type), "\n", sep = "")
  cat("\n")

  cat("  Catheters:\n")
  cat("    Total                                       : ", catheters$n_catheters, "\n", sep = "")
  cat("    Procedure type                              : ", fmt_demo(catheters$procedure_type), "\n", sep = "")
  cat("    Insertion to PD start (days, Q1/median/Q3)  : ",
      fmt_quartiles(catheters$insertion_to_start_days), "\n", sep = "")
  cat("    PD start to infection (days, Q1/median/Q3)  : ",
      fmt_quartiles(catheters$start_to_infection_days), "\n", sep = "")
  cat("\n")

  cat("  Infections:\n")
  cat("    Organisms cultured                   : ", fmt_demo(infections$organisms), "\n", sep = "")
  cat("    PE outcome                           : ", fmt_demo(infections$pe_outcomes), "\n", sep = "")
  cat("    Culture-negative peritonitis         : ",
      fmt_n_pct(infections$culture_negative_n, infections$culture_negative_pct), "\n", sep = "")
  cat("    Peritonitis-related catheter removal : ",
      fmt_n_pct(infections$peritonitis_removal_n, infections$peritonitis_removal_pct), "\n", sep = "")
  cat("\n")

  cat("  Cohort : ", x$n_patients, " patients (",
      cohort[["incident"]], " incident, ", cohort[["prevalent"]], " prevalent)\n", sep = "")

  invisible(list(
    rate = rate$rate, rate_num = rate$rate_num, rate_den = rate$rate_den,
    rate_benchmark = rate$rate_benchmark, rate_met = rate$rate_met,
    pf = pf$pf, pf_num = pf$pf_num, pf_den = pf$pf_den,
    pf_benchmark = pf$pf_benchmark, pf_met = pf$pf_met,
    episode_types = episode_types,
    outcomes = outcomes,
    cohort = cohort,
    median_age_years = median_age_years,
    demographics = demographics,
    catheters = catheters,
    infections = infections
  ))
}




## User-facing helper for pd_unit object on separate ingest.R file


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
  cat("<pd_unit>", if (is.na(x$unit_id)) "(unnamed unit)" else x$unit_id, "\n")
  cat("  Reporting period : ", format(x$t0), " to ", format(x$t1), "\n", sep = "")
  cat("  Patients         : ", x$n_patients,
      " (", x$n_new, " incident)\n", sep = "")
  cat("  Catheters        : ", nrow(x$catheters), "\n", sep = "")
  cat("  Episodes         : ", nrow(x$infections), "\n", sep = "")
  cat("  Patient-years    : ", format(round(x$tpyar, 2), nsmall = 2), "\n", sep = "")
  invisible(x)
}


## User-facing helper for pd_unit object on separate ingest.R file

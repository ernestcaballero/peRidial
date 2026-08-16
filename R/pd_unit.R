
#' Create a pd_unit object
#'
#' Wraps a tibble/data frame with metadata about the name of PD unit,
#' the reporting period, etc
#'
#' @param unit_id Character. The name of PD Unit (e.g. "XYZ PD Unit")
#' @param period_start Date. Start date of reporting period \code{t_0}.
#' @param period_end Date. End date of reporting period \code{t_1}.
#' @param n_new Integer. Number of incident patients in the reporting period.
#' @param n_patients Integer. Number of prevalent patients in the reporting period.
#' @param total_patient_yrs_at_risk Integer. The total amount of time (in years) that patients actively spend on PD while vulnerable to complications like peritonitis.
#' @param patients. Tibble. A dataframe of patients.
#' @param catheters Tibble. A dataframe of catheter insertions.
#' @param infections Tibble. A dataframe of peritonitis infections.
#' @param patient_list List. A list of patient objects.
#'
#' @return An object of class \code{pd_unit}.
#' @export
#'
#' @examples
#' # example code
#'

new_pd_unit <- function(unit_id = NA_character_,
                        t0 = as.Date(NA),        # start date
                        t1 = as.Date(NA),        # end date
                        n_new = NA_integer_,
                        n_patients = NA_integer_,
                        tpyar = NA_integer_,     # total patient-years-at-risk
                        patients = tibble::tibble(),
                        catheters = tibble::tibble(),
                        infections = tibble::tibble(),
                        patient_list = list()) {

  stopifnot(length(unit_id) == 1, is.character(unit_id) || is.na(unit_id))
  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))

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




#' ----- VALIDATOR -----
#'
#'
#'
#'

validate_pd_unit <- function(x) {
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
  # checks if n_new exceeds n_patients
  if (x$n_new > x$n_patients) {
    stop("n_new cannot exceed n_patients.")
  }
  # checks if total_patient_yrs_at_risk is missing
  # NOTE: match this field name exactly to whatever new_pd_unit() uses
  if (is.na(x$tpyar)) {
    stop("Total patient-years-at-risk is missing.")
  }
  # checks if tpyar is less than 0
  if (x$tpyar < 0) {
    stop("Total patient-years-at-risk cannot be negative.")
  }
  # checks nrow(patients) matches n_patients
  if (nrow(x$patients) > 0 && nrow(x$patients) != x$n_patients) {
    stop("Total number of patients does not match total rows in `patients`.")
  }
  # checks catheters reference a valid nhi
  if (nrow(x$catheters) > 0 && !all(x$catheters$nhi %in% x$patients$nhi)) {
    stop("Some catheter records reference an nhi not present in `patients`.")
  }
  # checks infections reference a valid catheter_id
  if (nrow(x$infections) > 0 && !all(x$infections$catheter_id %in% x$catheters$catheter_id)) {
    stop("Some infection records reference a catheter_id not present in `catheters`.")
  }
  # checks patient_list length matches n_patients
  if (length(x$patient_list) != x$n_patients) {
    stop("Length of `patient_list` does not match n_patients.")
  }

  x
}

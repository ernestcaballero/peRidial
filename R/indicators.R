#' Calculate exposure days between two dates
#'
#' Computes the number of days a patient is active on PD (at risk)
#' between two dates. Intended to be called with dates that have been
#' truncated/censored to the reporting window.
#'
#' @param min_date Date. The truncated start of the exposure interval.
#' @param max_date Date. The truncated end of the exposure interval.
#'
#' @return A non-negative numeric vector of exposure days.
#' @export
#'
exposure_days <- function(min_date, max_date) {
  stopifnot(inherits(min_date, "Date"), inherits(max_date, "Date"))
  as.numeric(pmax(0, max_date - min_date))
}



#' Calculate total patient-years-at-risk for a unit
#'
#' Computes the total exposure time, in patient-years, across all catheter
#' episodes for a unit. Exposure days for each episode are calculated via
#' \code{exposure_days()}, summed per patient (\code{patient_id}) using
#' \code{rowsum()}, and converted to years using a 365.25-day year.
#'
#' @param catheters Data frame. One row per catheter episode, with columns
#'   \code{patient_id} (patient identifier), \code{pd_start_date} (Date), and
#'   \code{pd_stop_date} (Date). Assumed to already be validated by the object's
#'   constructor.
#'
#' @return A single non-negative numeric value: total patient-years-at-risk
#'   across all patients.
#' @export
#'
total_patient_years <- function(catheters) {
  stopifnot(is.data.frame(catheters))
  stopifnot(all(c("patient_id", "pd_start_date", "pd_stop_date") %in% names(catheters)))
  catheters$exp_days <- exposure_days(catheters$pd_start_date, catheters$pd_stop_date)
  patient_years <- rowsum(catheters$exp_days, catheters$patient_id) / 365.25
  return(sum(patient_years))
}

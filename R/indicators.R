#' Calculate exposure days between two dates
#'
#' Computes the number of days a patient is active on PD (at risk)
#' between two dates. Intended to be called with dates that have been
#' truncated/censored to the reporting window.
#'
#' @param min_date Date. The truncated start of the exposure interval (e.g. \code{t0} or
#'    start date of PD therapy.
#' @param max_date Date. The truncated end of the exposure interval (e.g. \code{t1},
#'    death, transplant, or permanent HD transfer).
#'
#' @return A non-negative numeric vector of exposure days.
#' @export
#'
exposure_days <- function(min_date, max_date) {
  stopifnot(inherits(min_date, "Date"), inherits(max_date, "Date"))
  as.numeric(pmax(0, max_date - min_date + 1))
}



#' Days a single catheter was at risk within the reporting period
#'
#' Days a catheter is at risk: this catheter's PD window clipped to
#' the reporting period \code{[t0, t1]} and censored at the patient's
#' \code{tau} (death, transplant, or permanent transfer to HD). The clipped
#' interval is then measured by \code{exposure_days()}, which is documented as
#' taking dates that have already been truncated.
#'
#' An \code{NA} \code{pd_stop_date} means the catheter is still in active
#' use, and an \code{NA} \code{tau} means the patient never left PD, so neither
#' contributes an upper bound and \code{t1} closes the interval instead.
#'
#' @param catheter A \code{pd_catheter} object (or any list carrying
#'   \code{pd_start_date} and \code{pd_stop_date}).
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param tau Date. The date this catheter's patient permanently left PD, or
#'   \code{NA} if they did not. Supplied from \code{pd_patient()}'s
#'   \code{transfer_date}.
#'
#' @return A single non-negative numeric: days at risk, inclusive of both
#'   endpoints. Zero for a catheter whose window never overlaps the period.
#' @export
#'
catheter_exposure_days <- function(catheter, t0, t1, tau = as.Date(NA)) {
  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"), inherits(tau, "Date"))
  stopifnot(inherits(catheter$pd_start_date, "Date"))

  # Latest of (catheter start, period start); earliest of (catheter stop, period end, tau), ignoring the bounds that aren't set.
  start <- max(catheter$pd_start_date, t0)
  end <- min(c(catheter$pd_stop_date, t1, tau), na.rm = TRUE)

  exposure_days(start, end)
}



#' Calculate total patient-years-at-risk for a unit
#'
#' Computes the denominator for the unit's peritonitis rate: total exposure
#' time in patient-years across every catheter in the cohort, censored to the
#' reporting period and to each patient's own \code{tau}, and converted to
#' years using a 365.25-day year.
#'
#' This takes \code{patient_list} rather than a raw catheters data frame
#' because censoring is a patient-level property: \code{tau} lives on
#' \code{pd_patient} (as \code{transfer_date}), while the PD windows live on
#' the \code{pd_catheter} objects nested inside it. Walking the objects keeps
#' each catheter paired with the \code{tau} that applies to it.
#'
#' @param patient_list List of \code{pd_patient} objects, each carrying its
#'   own \code{catheters} and \code{transfer_date}. Assumed already validated
#'   by the objects' constructors.
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#'
#' @return A single non-negative numeric value: total patient-years-at-risk
#'   across all patients.
#' @export
#'
total_patient_years <- function(patient_list, t0, t1) {
  stopifnot(is.list(patient_list))
  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))
  if (is.na(t0) || is.na(t1)) {
    stop("Both t0 and t1 must be supplied to scope patient-years-at-risk.")
  }
  if (t0 > t1) {
    stop("t0 must be on or before t1.")
  }
  if (length(patient_list) == 0) {
    return(0)
  }

  days <- vapply(patient_list, function(p) {
    if (length(p$catheters) == 0) {
      return(0)
    }
    tau <- p$transfer_date
    if (is.null(tau)) tau <- as.Date(NA)

    sum(vapply(p$catheters, catheter_exposure_days, numeric(1),
               t0 = t0, t1 = t1, tau = tau))
  }, numeric(1))

  sum(days) / 365.25
}

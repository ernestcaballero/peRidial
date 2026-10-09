
#' Calculate exposure days between two dates
#'
#' Computes the number of days a patient is active on PD (at risk) between two dates.
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

  # Latest of (catheter start, period start);
  # Earliest of (catheter stop, period end, tau)
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




#' Peritonitis rate by calendar period (Monthly, Quarterly, Yearly)
#'
#' A generalised function that bins a unit's reporting period `[t0, t1]`
#' into calendar months, quarters, or years and recalculates the peritonitis rate
#' (countable episodes / patient-years at risk) within each bin.
#' `total_patient_years()` is used for the denominator and the same numerator rule
#' (`infections$counts_toward_rate`) as `summary.pd_unit()`. Partial periods
#' are clipped to `t0`/`t1` rather than padded out to a full period,
#' so `patient_years` never exceeds what the unit could actually accrue.
#'
#' @param x A `pd_unit` object.
#' @param cal.period Character. `"month"`, `"quarter"`, or `"year"`.
#'
#' @return A tibble, one row per period spanned by `[x$t0, x$t1]`:
#'   `period` (Date, start of that period),
#'   `label` (character, e.g. `"Mar 2025"`, `"Q1 2025"`, `"2025"`),
#'   `period_start`/`period_end` (the bin's actual bounds, clipped to `[t0, t1]`),
#'   `n_episodes` (countable episodes with an `infection_date` in the bin),
#'   `patient_years` (denominator from `total_patient_years()`), and
#'   `rate` (`n_episodes / patient_years`, `NA` when `patient_years` is 0).
#' @export
#'
#' @examples
#' x <- new_pd_unit(
#'   unit_id = "Wellington PD Unit",
#'   t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
#'   n_new = 0L, n_patients = 0L, tpyar = 0
#' )
#' peritonitis_rate_by_period(x, cal.period = "quarter")
#'
peritonitis_rate_by_period <- function(x, cal.period = c("month", "quarter", "year")) {
  stopifnot(inherits(x, "pd_unit"))
  cal.period <- match.arg(cal.period)
  if (is.na(x$t0) || is.na(x$t1)) {
    stop("Both t0 and t1 must be supplied to bin a peritonitis rate by period.")
  }

  # start of the period (month/quarter/year) containing the date
  floor_to_period <- function(date) {
    switch(cal.period,
           month = as.Date(format(date, "%Y-%m-01")),
           quarter = {
             qm <- (as.integer(format(date, "%m")) - 1) %/% 3 * 3 + 1   # collapses months 1–3 to 0, 4–6 to 1, etc, then converts it back to the starting month of that quarter
             as.Date(sprintf("%s-%02d-01", format(date, "%Y"), qm))},
           year = as.Date(format(date, "%Y-01-01"))
    )
  }
  # builds the text label for the bin (example: May ("05") gives (5-1) %/% 3 + 1 = 2, producing "Q2 2025")
  label_for <- function(date) {
    switch(cal.period,
           month = format(date, "%b %Y"),
           quarter = paste0("Q", (as.integer(format(date, "%m")) - 1) %/% 3 + 1,
                            " ", format(date, "%Y")),
           year = format(date, "%Y")
    )
  }

  # lay out the bin boundaries
  bin_starts <- seq(floor_to_period(x$t0), x$t1, by = cal.period)
  bin_ends <- c(bin_starts[-1] - 1, x$t1)

  period_start <- pmax(bin_starts, x$t0)
  period_end   <- pmin(bin_ends, x$t1)  # each bin's end is one day before the next bin's start

  # count the episodes per bin
  n_episodes <- vapply(seq_along(bin_starts), function(i) {
    if (nrow(x$infections) == 0) return(0L)
    sum(x$infections$counts_toward_rate &
          x$infections$infection_date >= period_start[i] &
          x$infections$infection_date <= period_end[i],
        na.rm = TRUE)
  }, integer(1))

  patient_years <- vapply(seq_along(bin_starts), function(i) {
    total_patient_years(x$patient_list, period_start[i], period_end[i])
  }, numeric(1))

  rate <- ifelse(patient_years == 0, NA_real_, n_episodes / patient_years)

  tibble::tibble(
    period = bin_starts,
    label = label_for(bin_starts),
    period_start = period_start,
    period_end = period_end,
    n_episodes = n_episodes,
    patient_years = patient_years,
    rate = rate
  )
}





#' Peritonitis rate by calendar month
#'
#' A wrapper around `peritonitis_rate_by_period(x, cal.period = "month")`.
#' Monthly peritonitis rate with a`month` column instead of `period` and `label` columns.
#'
#' @param x A `pd_unit` object.
#'
#' @return A tibble, one row per calendar month spanned by `[x$t0, x$t1]`:
#'   `month` (Date, first of that month),
#'   `period_start`/`period_end` (the bin's actual bounds, clipped to `[t0, t1]`),
#'   `n_episodes`,
#'   `patient_years`, and
#'   `rate`.
#'   See `peritonitis_rate_by_period()` for the full details of how each bin is computed.
#' @export
#'
#' @examples
#' x <- new_pd_unit(
#'   unit_id = "Wellington PD Unit",
#'   t0 = as.Date("2025-01-01"), t1 = as.Date("2025-03-31"),
#'   n_new = 0L, n_patients = 0L, tpyar = 0
#' )
#' monthly_peritonitis_rate(x)
#'
monthly_peritonitis_rate <- function(x) {
  out <- peritonitis_rate_by_period(x, cal.period = "month")
  tibble::tibble(
    month = out$period,
    period_start = out$period_start,
    period_end = out$period_end,
    n_episodes = out$n_episodes,
    patient_years = out$patient_years,
    rate = out$rate
  )
}

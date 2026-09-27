
#' Peritonitis rate broken down by calendar month
#'
#' Bins a unit's reporting period `[t0, t1]` into calendar months and
#' recomputes the peritonitis rate (countable episodes / patient-years at
#' risk) within each bin, reusing `total_patient_years()` for the
#' denominator and the same numerator rule (`infections$counts_toward_rate`)
#' as `summary.pd_unit()`. Partial months at either end of the reporting
#' period are clipped to `t0`/`t1` rather than padded to a full month, so
#' `patient_years` never exceeds what the unit could actually accrue.
#'
#' @param x A `pd_unit` object.
#'
#' @return A tibble, one row per calendar month spanned by `[x$t0, x$t1]`:
#'   `month` (Date, first of that month), `period_start`/`period_end` (the
#'   bin's actual bounds, clipped to `[t0, t1]`), `n_episodes` (countable
#'   episodes with an `infection_date` in the bin), `patient_years`
#'   (denominator from `total_patient_years()`), and `rate`
#'   (`n_episodes / patient_years`, `NA` when `patient_years` is 0).
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
  # one bin per calendar month at [t0, t1]; seq() stops at/before t1
  month_starts <- seq(as.Date(format(x$t0, "%Y-%m-01")), x$t1, by = "month")
  # each bin runs to the day before the next bin starts; the last bin (and a single-month period) runs to t1
  month_ends <- c(month_starts[-1] - 1, x$t1)

  # clip to the actual reporting window, not the calendar month
  period_start <- pmax(month_starts, x$t0)
  period_end <- pmin(month_ends, x$t1)

  n_episodes <- vapply(seq_along(month_starts), function(i) {
    if (nrow(x$infections) == 0)
      return(0L)
    sum(x$infections$counts_toward_rate &
          x$infections$infection_date >= period_start[i] &
          x$infections$infection_date <= period_end[i], na.rm = TRUE)
  }, integer(1))

  patient_years <- vapply(seq_along(month_starts), function(i) {
    total_patient_years(x$patient_list, period_start[i], period_end[i])
  }, numeric(1))

  rate <- ifelse(patient_years == 0, NA_real_, n_episodes / patient_years)

  tibble::tibble(
    month = month_starts,
    period_start = period_start,
    period_end = period_end,
    n_episodes = n_episodes,
    patient_years = patient_years,
    rate = rate
  )
}


#' Peritonitis rate by calendar period (Monthly, Quarterly, Yearly)
#'
#' A generalised function that bins a unit's reporting period `[t0, t1]`
#' into calendar months, quarters, or years and recalculates the peritonitis rate
#' (countable episodes / patient-years at risk) within each bin.
#' `total_patient_years()` is used for the denominator and the same numerator rule
#' (`infections$counts_toward_rate`) as `summary.pd_unit()`. Partial periods
#' at either end of the reporting window are clipped to `t0`/`t1` rather
#' than padded out to a full period, so `patient_years` never exceeds what
#' the unit could actually accrue.
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
#' A wrapper around `peritonitis_rate_by_period(x, cal.period = "month")`,
#' kept for backwards compatibility and for callers who only want the
#' monthly view without the generic `period`/`label` columns.
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


#' Plot a pd_unit object
#'
#' Draws the unit's peritonitis rate trend, binned monthly, quarterly, or
#' yearly (`type`), with each bin computed by `peritonitis_rate_by_period()`,
#' the rate value printed above each bar, and the unit's ISPD benchmark rate
#' drawn as a reference line. Requires the ggplot2 package.
#'
#' @param x A `pd_unit` object.
#' @param y Ignored (required by the `plot()` generic).
#' @param type Character. `"monthly"`, `"quarterly"`, or `"yearly"`.
#' @param benchmark Numeric. The ISPD peritonitis-rate benchmark to draw as
#'   a reference line, in episodes per patient-year. Defaults to the unit's
#'   own `x$rate_benchmark` (set when the `pd_unit` was created, via
#'   `new_pd_unit()`/`pd_unit()`'s `rate_benchmark` argument, default=0.40 unless
#'   overriden there) -- the same value `summary.pd_unit()` judges against.
#'   Pass a value here to override it for this plot only.
#' @param ... Ignored.
#'
#' @return A `ggplot` object, invisibly plotted.
#' @export
#'
#' @examples
#' \donttest{
#' x <- new_pd_unit(
#'   unit_id = "Wellington PD Unit",
#'   t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
#'   n_new = 0L, n_patients = 0L, tpyar = 0)
#'
#'   plot(x, type = "monthly")
#'   plot(x, type = "quarterly")
#'   plot(x, type = "yearly")
#' }
#'
plot.pd_unit <- function(x, y,
                         type = c("monthly", "quarterly", "yearly"),
                         benchmark = x$rate_benchmark, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" is required for plot.pd_unit(). ",
         "Install it with install.packages(\"ggplot2\").", call. = FALSE)
  }
  type <- match.arg(type)

  unit <- switch(type,
    monthly = "month",
    quarterly = "quarter",
    yearly = "year"
  )
  rate_df <- peritonitis_rate_by_period(x, cal.period = unit)

  # converts labels as factors and keep current order
  rate_df$label <- factor(rate_df$label, levels = rate_df$label)

  period_word <- switch(type,
    monthly = "Monthly", quarterly = "Quarterly", yearly = "Yearly")
  x_label <- switch(type,
    monthly = "Month", quarterly = "Quarter", yearly = "Year")
  unit_label <- if (is.na(x$unit_id)) "Unnamed unit" else x$unit_id

  # label bins that actually have a rate
  labelled <- rate_df[!is.na(rate_df$rate), ]

  p <- ggplot2::ggplot(rate_df, ggplot2::aes(x = .data$label, y = .data$rate)) +
    ggplot2::geom_col(fill = "#2C7FB8", na.rm = TRUE) +
    ggplot2::geom_text(
      data = labelled,
      ggplot2::aes(label = sprintf("%.2f", .data$rate)),
      vjust = -0.5, size = 3.2
    ) +
    ggplot2::geom_hline(yintercept = benchmark, linetype = "dashed",
                        colour = "firebrick") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = paste(period_word, "Peritonitis Rate:", unit_label),
      subtitle = paste0(format(x$t0), " to ", format(x$t1),
                        "  (dashed line: ISPD benchmark, ", benchmark,
                        " episodes/patient-year)"),
      x = x_label, y = "Peritonitis rate (episodes / patient-year)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  print(p)
  invisible(p)
}

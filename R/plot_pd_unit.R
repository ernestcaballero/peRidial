
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
#' Draws one of several views of a `pd_unit` object, selected by `type`:
#' `"monthly"`/`"quarterly"` draw a peritonitis rate trend binned by that
#' calendar period (via `peritonitis_rate_by_period()`), with the rate
#' value printed above each bar and the unit's ISPD benchmark rate drawn as
#' a reference line;
#' `"yearly"` draws a **combined, two-panel figure**: a cumulative (year-to-date)
#' rate line on top and a cumulative (year-to-date) peritonitis-free percentage
#' line underneath, each on its own correctly-scaled axis. Both lines are
#' built by reusing the monthly bins and running a `cumsum()`/running
#' per-patient tally. Both views reuse `summarise_rate()`/`summarise_pf()`
#' -- the same internal helpers `summary.pd_unit()` calls -- rather than recalculating those numbers.
#' Requires the ggplot2 package, and also patchwork for `type = "yearly"`.
#'
#' @param x A `pd_unit` object.
#' @param y Ignored (required by the `plot()` generic).
#' @param type Character. `"monthly"`, `"quarterly"`, or `"yearly"` (a
#'   cumulative year-to-date line, not a calendar-year bar).
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

  if (type == "yearly") {
    return(plot_cumulative_rate(x, benchmark))
  }

  unit <- switch(type,
    monthly = "month",
    quarterly = "quarter"
  )
  rate_df <- peritonitis_rate_by_period(x, cal.period = unit)

  # converts labels as factors and keep current order
  rate_df$label <- factor(rate_df$label, levels = rate_df$label)

  period_word <- switch(type,
    monthly = "Monthly", quarterly = "Quarterly")
  x_label <- switch(type,
    monthly = "Month", quarterly = "Quarter")
  unit_label <- if (is.na(x$unit_id)) "Unnamed unit" else x$unit_id

  # label bins that actually have a rate
  labelled <- rate_df[!is.na(rate_df$rate), ]

  # whole-period headline rate from reuse summarise_rate()
  overall <- summarise_rate(x)
  overall_label <- if (is.na(overall$rate)) {
    "Overall rate (whole period): NA"
  } else {
    sprintf("Overall rate (whole period): %.3f episode/patient-yr [%s]",
            overall$rate, if (overall$rate_met) "MET" else "NOT MET")
  }

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
      subtitle = paste0(format(x$t0), " to ", format(x$t1), "   |   ", overall_label,
                        "\n(dashed line: reference benchmark, ", benchmark,
                        " episodes/patient-year)"),
      x = x_label, y = "Peritonitis rate (episodes / patient-year)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  print(p)
  invisible(p)
}


#' Cumulative (year-to-date) peritonitis-free percentage, by calendar month
#'
#' For each calendar month spanned by `[x$t0, x$t1]`, computes the
#' percentage of the unit's whole-period cohort (`x$patients$patient_id`)
#' who have *not yet* had a countable peritonitis episode (`infections$counts_toward_rate`)
#' by the end of that month. A patient who has had an episode can't become
#' peritonitis-free again within the same reporting period, so this is
#' necessarily non-increasing month to month, and its final value equals
#' the same whole-period PF `summarise_pf()`/`summary.pd_unit()` report.
#'
#' @param x A `pd_unit` object.
#'
#' @return A tibble: `period` (Date, first of each month), `label`
#'   (character month label), `period_end` (Date, the bin's end, clipped to
#'   `x$t1`), and `cum_pf_pct` (numeric, 0-100; `NA` when the cohort is
#'   empty, i.e. `x$n_patients == 0`).
#' @noRd
#'
cumulative_pf_by_month <- function(x) {
  bins <- peritonitis_rate_by_period(x, cal.period = "month")
  # checks if an empty cohort carries a column-less `patients` tibble; fall back to an empty id set instead
  all_ids <- if ("patient_id" %in% names(x$patients)) {
    x$patients$patient_id
  } else {
    character(0)
  }
  n <- x$n_patients

  cum_pf_pct <- vapply(bins$period_end, function(period_end) {
    if (n == 0) {
      return(NA_real_)
    }
    if (nrow(x$infections) == 0) {
      return(100)
    }
    had_episode <- x$infections$counts_toward_rate &
      x$infections$infection_date <= period_end
    had_episode[is.na(had_episode)] <- FALSE
    non_pf_ids <- unique(x$infections$patient_id[had_episode])
    pf_count <- n - length(intersect(non_pf_ids, all_ids))
    pf_count / n * 100
  }, numeric(1))

  tibble::tibble(
    period = bins$period,
    label = bins$label,
    period_end = bins$period_end,
    cum_pf_pct = cum_pf_pct
  )
}


#' Draw the combined year-to-date rate and PF% panels for a pd_unit object
#'
#' Two stacked panels, each on its own correctly-scaled axis.
#' The top panel is the cumulative (year-to-date) peritonitis rate, built
#' by accumulating each month's episodes and patient-years from
#' `peritonitis_rate_by_period()` (`cumsum(n_episodes) / cumsum(patient_years)`)
#' -- the same episodes-per-patient-year units as the ISPD rate benchmark,
#' so the two stay directly comparable on one axis, and its last point
#' equals the whole-period rate by construction; the bottom panel is the
#' cumulative PF%. Both panels reuse `summarise_rate()`/`summarise_pf()` --
#' the same internal helpers `summary.pd_unit()` calls -- for their
#' reference benchmarks and final-point labels. Requires the patchwork
#' package.
#'
#' @param x A `pd_unit` object.
#' @param benchmark Numeric. The ISPD peritonitis-rate benchmark to draw as
#'   a reference line on the rate panel.
#'
#' @return A `patchwork` object, invisibly plotted as a side effect.
#' @noRd
#'
plot_cumulative_rate <- function(x, benchmark) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for plot.pd_unit(type = \"yearly\"). ",
         "Install it with install.packages(\"patchwork\").", call. = FALSE)
  }

  unit_label <- if (is.na(x$unit_id)) "Unnamed unit" else x$unit_id

  # -- rate panel -----------------------------------------------------
  monthly <- peritonitis_rate_by_period(x, cal.period = "month")
  monthly$label <- factor(monthly$label, levels = monthly$label)

  # get cumulative (year-to-date) rate
  overall <- summarise_rate(x)
  monthly$cum_episodes <- cumsum(monthly$n_episodes)
  monthly$cum_patient_years <- cumsum(monthly$patient_years)
  monthly$cum_rate <- ifelse(monthly$cum_patient_years == 0, NA_real_,
                             monthly$cum_episodes / monthly$cum_patient_years)

  # label only the last point
  last_rate_row <- monthly[nrow(monthly), ]
  last_rate_row$rate_label <- if (is.na(overall$rate)) {
    "NA"
  } else {
    sprintf("%.3f", overall$rate)
  }

  # y-axis must cover both the data and the benchmark line
  rate_y_max <- suppressWarnings(max(c(monthly$cum_rate, benchmark), na.rm = TRUE))
  if (!is.finite(rate_y_max) || rate_y_max <= 0) {
    rate_y_max <- if (is.finite(benchmark) && benchmark > 0) benchmark else 1
  }

  p_rate <- ggplot2::ggplot(monthly, ggplot2::aes(x = .data$label, y = .data$cum_rate,
                                                  group = 1)) +
    ggplot2::geom_line(colour = "#2C7FB8", linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(colour = "#2C7FB8", size = 1.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = benchmark, linetype = "dashed",
                        colour = "firebrick") +
    ggplot2::geom_text(
      data = last_rate_row,
      ggplot2::aes(label = .data$rate_label),
      vjust = -0.8, size = 3.4, na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(limits = c(0, rate_y_max * 1.15),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(
      title = "Cumulative (Year-to-Date) Peritonitis Rate",
      x = NULL, y = "Rate (episodes/patient-yr)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  # -- PF panel ---------------------------------------------------------
  pf_monthly <- cumulative_pf_by_month(x)
  pf_monthly$label <- factor(pf_monthly$label, levels = pf_monthly$label)
  pf_benchmark_pct <- summarise_pf(x)$pf_benchmark * 100

  last_pf_row <- pf_monthly[nrow(pf_monthly), ]
  last_pf_row$pf_label <- if (is.na(last_pf_row$cum_pf_pct)) {
    "NA"
  } else {
    sprintf("%.1f%%", last_pf_row$cum_pf_pct)
  }

  p_pf <- ggplot2::ggplot(pf_monthly, ggplot2::aes(x = .data$label, y = .data$cum_pf_pct,
                                                   group = 1)) +
    ggplot2::geom_line(colour = "#2CA25F", linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(colour = "#2CA25F", size = 1.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = pf_benchmark_pct, linetype = "dashed",
                        colour = "firebrick") +
    ggplot2::geom_text(
      data = last_pf_row,
      ggplot2::aes(label = .data$pf_label),
      vjust = -0.8, size = 3.4, na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 100),
                                expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Cumulative (Year-to-Date) Peritonitis-Free %",
      x = "Month", y = "Peritonitis-free (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # -- combine: two panels, two independent axes, never one dual-axis chart
  combined <- patchwork::wrap_plots(p_rate, p_pf, ncol = 1) +
    patchwork::plot_annotation(
      title = paste("Year-to-Date Indicator:", unit_label),
      subtitle = paste0(format(x$t0), " to ", format(x$t1),
                        "  (dashed lines: ISPD benchmark)")
    )

  print(combined)
  invisible(combined)
}

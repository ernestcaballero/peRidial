
#' Plot a pd_unit object
#'
#' Draws a plot of peritonitis rate and peritonitis-free percentage of a
#' `pd_unit` object, selected by `type`: `"monthly"`/`"quarterly"`
#' draw a peritonitis rate trend binned by that calendar period
#' (via `peritonitis_rate_by_period()`); `"yearly"` draws the cumulative
#' (year-to-date) rate  and a cumulative (year-to-date) peritonitis-free percentage
#' in a two-panel plot. Both views reuse `summarise_rate()`/`summarise_pf()`.
#' Requires the `ggplot2` package, and also `patchwork` for `type = "yearly"`.
#'
#' With `type = "yearly"`, four optional bar-chart panels summarising the
#' reporting cohort are added underneath the two cumulative panels; each is
#' off by default and is switched on with its own `show_*` argument. They take
#' their data from the unit's tibbles (`x$patients`, `x$catheters`) and
#' `x$n_new`/`x$n_patients`. A patient with a missing value is counted in an
#' `"Unknown"` bar.
#'
#' @param x A `pd_unit` object.
#' @param y Ignored (required by the `plot()` generic).
#' @param type Character. `"monthly"`, `"quarterly"`, or `"yearly"` (a
#'   cumulative year-to-date line, not a calendar-year bar).
#' @param benchmark Numeric. The ISPD peritonitis-rate benchmark to draw as
#'   a reference line, in episodes per patient-year. Defaults to the unit's
#'   own `x$rate_benchmark` (set when the `pd_unit` was created, via
#'   `new_pd_unit()`/`pd_unit()`'s `rate_benchmark` argument, default 0.40 unless
#'   overridden there) -- the same value `summary.pd_unit()` judges against.
#'   Pass a value here to override it for this plot only.
#' @param show_ip Logical. Add a bar chart of incident vs prevalent patients
#'   (`x$n_new` and `x$n_patients - x$n_new`, as a percentage of the cohort),
#'   side by side. Only for `type = "yearly"`. Default `FALSE`.
#' @param show_pkd Logical. Add a horizontal bar chart of primary kidney
#'   disease (`x$patients$primary_kidney_disease`), as a percentage of the
#'   cohort, most common first. Only for `type = "yearly"`. Default `FALSE`.
#' @param show_modality Logical. Add a bar chart of PD modality
#'   (`x$patients$dialysis_type`: CAPD, APD or Hybrid), as a percentage of the
#'   cohort. Only for `type = "yearly"`. Default `FALSE`.
#' @param show_stop.reason Logical. Add a horizontal bar chart of the reasons
#'   PD catheters were stopped, as a **count of catheters** (a patient can have
#'   more than one). Only catheters whose `pd_stop_date` falls within
#'   `[x$t0, x$t1]` are counted. The reason is the catheter's `removal_reason`
#'   checked against the accepted list of reasons in `R/value_maps.R`
#'   (the ISPD reasons plus death, transplant and permanent transfer to HD);
#'   text not on the list is counted as `"Other"`.
#'   Where a catheter was removed for a permanent transfer to HD, the patient's
#'   modality-change reason (`x$patients$transfer_detail`) is checked against
#'   the same list and used instead. Catheters with no reason are counted as
#'   `"Unknown"`. Only categories with a non-zero count are drawn, most common
#'   first. Only for `type = "yearly"`. Default `FALSE`.
#' @param ... Ignored.
#'
#' @return Invisibly, the plot that was drawn: a `ggplot` object for
#'   `"monthly"`/`"quarterly"`, or a `patchwork` object for `"yearly"`.
#'   The plot is also printed.
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
#'
#'   # extra cohort panels (yearly only); this empty unit gives a warning
#'   # and blank "No data" panels
#'   plot(x, type = "yearly", show_ip = TRUE, show_pkd = TRUE,
#'        show_modality = TRUE, show_stop.reason = TRUE)
#' }
#'
plot.pd_unit <- function(x, y,
                         type = c("monthly", "quarterly", "yearly"),
                         benchmark = x$rate_benchmark,
                         show_ip = FALSE, show_pkd = FALSE,
                         show_modality = FALSE, show_stop.reason = FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" is required for plot.pd_unit(). ",
         "Install it with install.packages(\"ggplot2\").")
  }
  type <- match.arg(type)

  # each show_* must be a single TRUE/FALSE, and none may be TRUE unless type = "yearly"
  show_args <- list(show_ip = show_ip, show_pkd = show_pkd,
                    show_modality = show_modality,
                    show_stop.reason = show_stop.reason)
  for (nm in names(show_args)) {
    v <- show_args[[nm]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      stop("`", nm, "` must be a single TRUE or FALSE.")
    }
  }
  if (type != "yearly" && any(unlist(show_args))) {
    stop("`show_ip`, `show_pkd`, `show_modality` and `show_stop.reason` ",
         "can only be used with type = \"yearly\".")
  }

  if (type == "yearly") {
    extras <- build_extra_panels(x, show_ip, show_pkd, show_modality,
                                 show_stop.reason)
    return(plot_cumulative_rate(x, benchmark, extras))
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
    "Overall rate: NA"
  } else {
    sprintf("Overall rate: %.3f episode/patient-yr [%s]",
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
      subtitle = paste0(format(x$t0), " to ", format(x$t1), "\n",
                        overall_label),
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
#' @param extras List of `ggplot` panels (from `build_extra_panels()`) laid out
#'   in a two-column grid underneath the two cumulative panels. Empty by default.
#'
#' @return A `patchwork` object, invisibly plotted as a side effect.
#' @noRd
#'
plot_cumulative_rate <- function(x, benchmark, extras = list()) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package \"patchwork\" is required for plot.pd_unit(type = \"yearly\"). ",
         "Install it with install.packages(\"patchwork\").", call. = FALSE)
  }

  unit_label <- if (is.na(x$unit_id)) "Unnamed unit" else x$unit_id

  # Rate panel
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

  # Peritonitis-free percentage panel
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
  top <- patchwork::wrap_plots(p_rate, p_pf, ncol = 1)
  if (length(extras) > 0) {
    # optional cohort panels sit in a two-column grid under the cumulative pair
    extras_grid <- patchwork::wrap_plots(extras, ncol = 2)
    body <- patchwork::wrap_plots(top, extras_grid, ncol = 1,
                                  heights = c(2, 1.2 * ceiling(length(extras) / 2)))
  } else {
    body <- top
  }
  combined <- body +
    patchwork::plot_annotation(
      title = paste("Year-to-Date Indicator:", unit_label),
      subtitle = paste0(format(x$t0), " to ", format(x$t1),
                        "  (dashed lines: ISPD benchmark)")
    )

  print(combined)
  invisible(combined)
}




# Optional cohort panels for plot.pd_unit(type = "yearly")

#' Build the optional cohort panels requested by the show_* arguments
#' @noRd
#'
build_extra_panels <- function(x, show_ip, show_pkd, show_modality,
                               show_stop.reason) {
  panels <- list()
  if (!any(show_ip, show_pkd, show_modality, show_stop.reason)) {
    return(panels)
  }

  empty <- isTRUE(x$n_patients == 0) || nrow(x$patients) == 0
  if (empty) {
    warning("This pd_unit has no patients, so the requested panels have no data to plot.")
  }

  if (show_ip) {
    panels$ip <- if (empty) blank_panel("Incident vs Prevalent Patients") else panel_incident_prevalent(x)
  }
  if (show_pkd) {
    panels$pkd <- if (empty) blank_panel("Primary Kidney Disease") else panel_pkd(x)
  }
  if (show_modality) {
    panels$modality <- if (empty) blank_panel("PD Modality") else panel_modality(x)
  }
  if (show_stop.reason) {
    panels$stop_reason <- if (empty) blank_panel("Reason PD Stopped (by catheter)") else panel_stop_reason(x)
  }
  panels
}


#' One column of the unit's patients tibble as character, or NA if it is absent
#' @noRd
patient_column <- function(x, name) {
  if (name %in% names(x$patients)) {
    as.character(x$patients[[name]])
  } else {
    rep(NA_character_, nrow(x$patients))
  }
}


#' Count a categorical vector for a bar chart
#'
#' Missing or blank values become "Unknown", which is always present (with a
#' count of 0 if need be). Without `levels` the observed categories are sorted
#' from most to least common. With `levels` those categories come first, in the
#' order given and kept even at 0, then any other observed categories, most
#' common first. "Unknown" is last in that case.
#'
#' @param values Character vector, one element per patient (or catheter).
#' @param denom Number the percentages are taken of.
#' @param levels Optional character vector of categories to always show.
#'
#' @return A tibble with `category`, `n` and `pct` (percentage of `denom`).
#' @noRd
tabulate_categories <- function(values, denom = length(values), levels = NULL) {
  values <- trimws(as.character(values))
  values[is.na(values) | !nzchar(values)] <- "Unknown"

  cats <- unique(values)
  n <- vapply(cats, function(v) sum(values == v), integer(1), USE.NAMES = FALSE)
  tab <- tibble::tibble(category = cats, n = n)

  absent <- setdiff(c(levels, "Unknown"), tab$category)
  if (length(absent) > 0) {
    tab <- rbind(tab, tibble::tibble(category = absent, n = 0L))
  }

  if (is.null(levels)) {
    tab <- tab[order(-tab$n, tab$category == "Unknown", tab$category), ]
  } else {
    rank <- match(tab$category, levels)
    rank[is.na(rank)] <- length(levels) + 1L
    rank[tab$category == "Unknown"] <- length(levels) + 2L
    tab <- tab[order(rank, -tab$n, tab$category), ]
  }

  tab$pct <- if (denom > 0) tab$n / denom * 100 else rep(NA_real_, nrow(tab))
  tab
}


#' A simple bar chart of a `tabulate_categories()` tibble
#'
#' @param tab Output of `tabulate_categories()`, already in plotting order.
#' @param title,subtitle Plot title and subtitle.
#' @param value `"pct"` (bars are % of the cohort, labelled like `12.5%`) or
#'   `"n"` (bars are counts).
#' @param horizontal If `TRUE`, draw horizontal bars with the first row on top.
#' @param fill Bar colour.
#'
#' @return A `ggplot` object.
#' @noRd
bar_panel <- function(tab, title, subtitle = NULL, value = c("pct", "n"),
                      horizontal = FALSE, fill = "#2C7FB8") {
  value <- match.arg(value)

  tab$y <- if (value == "pct") tab$pct else as.numeric(tab$n)
  tab$bar_label <- if (value == "pct") {
    ifelse(is.na(tab$pct), "NA", sprintf("%.1f%%", tab$pct))
  } else {
    as.character(tab$n)
  }
  # keep the plotting order; reversed when flipped so the first row ends up on top
  tab$category <- factor(tab$category,
                         levels = if (horizontal) rev(tab$category) else tab$category)

  # whole-number axis for counts
  y_breaks <- if (value == "n") {
    function(lims) {
      b <- pretty(lims)
      b[b == round(b)]
    }
  } else {
    ggplot2::waiver()
  }

  p <- ggplot2::ggplot(tab, ggplot2::aes(x = .data$category, y = .data$y)) +
    ggplot2::geom_col(fill = fill, na.rm = TRUE) +
    ggplot2::scale_y_continuous(breaks = y_breaks,
                                expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = title, subtitle = subtitle, x = NULL,
      y = if (value == "pct") "Patients (%)" else "Catheters stopped (n)"
    ) +
    ggplot2::theme_minimal()

  if (horizontal) {
    p + ggplot2::geom_text(ggplot2::aes(label = .data$bar_label),
                           hjust = -0.15, size = 3.2, na.rm = TRUE) +
      ggplot2::coord_flip()
  } else {
    p + ggplot2::geom_text(ggplot2::aes(label = .data$bar_label),
                           vjust = -0.5, size = 3.2, na.rm = TRUE) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
}


#' An empty panel with a "No data" note
#' @noRd
blank_panel <- function(title) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = "No data",
                      size = 5, colour = "grey50") +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}



#' Incident vs prevalent patients: one pair of side-by-side bars
#' @noRd
panel_incident_prevalent <- function(x) {
  n_patients <- x$n_patients
  groups <- c("Incident", "Prevalent")
  tab <- tibble::tibble(
    group = factor(groups, levels = groups),
    n = c(x$n_new, n_patients - x$n_new)
  )
  tab$pct <- tab$n / n_patients * 100
  tab$bar_label <- sprintf("%.1f%%", tab$pct)

  ggplot2::ggplot(tab, ggplot2::aes(x = .data$group, y = .data$pct, fill = .data$group)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$bar_label),
                       vjust = -0.5, size = 3.2) +
    ggplot2::scale_fill_manual(values = c(Incident = "#2C7FB8", Prevalent = "#7FCDBB"),
                               guide = "none") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Incident vs Prevalent Patients",
      subtitle = paste0("n = ", n_patients, " patients (", x$n_new, " incident)"),
      x = NULL, y = "Patients (%)"
    ) +
    ggplot2::theme_minimal()
}



#' Primary kidney disease in horizontal bars, with most common first
#' @noRd
panel_pkd <- function(x) {
  n_patients <- nrow(x$patients)
  tab <- tabulate_categories(patient_column(x, "primary_kidney_disease"),
                             denom = n_patients)
  bar_panel(tab, "Primary Kidney Disease",
            subtitle = paste0("n = ", n_patients, " patients"),
            horizontal = TRUE)
}



#' PD modality (CAPD / APD / Hybrid)
#' @noRd
panel_modality <- function(x) {
  n_patients <- nrow(x$patients)
  vals <- trimws(patient_column(x, "dialysis_type"))
  key <- tolower(vals)
  vals[key %in% "capd"] <- "CAPD"
  vals[key %in% "apd"] <- "APD"
  vals[key %in% "hybrid"] <- "Hybrid"

  tab <- tabulate_categories(vals, denom = n_patients,
                             levels = c("CAPD", "APD", "Hybrid"))
  bar_panel(tab, "PD Modality",
            subtitle = paste0("n = ", n_patients, " patients"))
}



#' Reason for each PD catheter stopped within the reporting period
#'
#' Counts catheters (not patients) whose `pd_stop_date` is within `[t0, t1]`.
#' The reason is the standardised `removal_reason`. Where that is a permanent
#' transfer to HD, it is replaced by the patient's modality-change reason
#' (`transfer_detail`), but only when that patient's censoring event really was
#' the transfer to HD, since `transfer_detail` holds a cause of death for a
#' death. A blank reason comes back as `NA` (shown as "Unknown").
#'
#' @param x A `pd_unit` object.
#'
#' @return A character vector, one element per stopped catheter.
#' @noRd
catheter_stop_reasons <- function(x) {
  cath <- x$catheters
  needed <- c("patient_id", "pd_stop_date", "removal_reason")
  if (nrow(cath) == 0 || !all(needed %in% names(cath))) {
    return(character(0))
  }

  stopped <- !is.na(cath$pd_stop_date) &
    cath$pd_stop_date >= x$t0 & cath$pd_stop_date <= x$t1
  cath <- cath[stopped, , drop = FALSE]
  reason <- standardise_removal_reason(cath$removal_reason)

  to_hd <- reason %in% "Permanent transfer to HD"
  if (any(to_hd)) {
    i <- match(cath$patient_id[to_hd], x$patients$patient_id)
    hd_patient <- tolower(patient_column(x, "transfer_reason")[i]) %in%
      "permanent transfer to hd"
    detail <- ifelse(hd_patient, patient_column(x, "transfer_detail")[i],
                     NA_character_)
    reason[to_hd] <- standardise_removal_reason(detail)
  }
  reason
}


#' Reasons PD catheters were stopped: horizontal bars, counts of catheters
#' @noRd
panel_stop_reason <- function(x) {
  reasons <- catheter_stop_reasons(x)
  tab <- tabulate_categories(reasons, denom = length(reasons))
  bar_panel(tab, "Reason PD Stopped (by catheter)",
            subtitle = paste0(length(reasons), " catheter",
                              if (length(reasons) != 1L) "s", " stopped, ",
                              format(x$t0), " to ", format(x$t1)),
            value = "n", horizontal = TRUE)
}

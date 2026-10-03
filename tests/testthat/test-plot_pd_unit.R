# Shared fixtures (T0, T1, make_catheter(), make_patient(), make_unit()) live
# in helper-fixtures.R, which testthat sources before every test file.

# peritonitis_rate_by_period() / monthly_peritonitis_rate()

test_that("peritonitis_rate_by_period() bins episodes and patient-years by month", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = as.Date("2025-03-10"),
    counts_toward_rate = TRUE
  ))
  rate_df <- peritonitis_rate_by_period(x, cal.period = "month")
  expect_equal(nrow(rate_df), 12L)
  expect_equal(sum(rate_df$n_episodes), 1L)
  expect_equal(rate_df$n_episodes[rate_df$period == as.Date("2025-03-01")], 1L)
  expect_equal(rate_df$label[rate_df$period == as.Date("2025-03-01")], "Mar 2025")
})

test_that("peritonitis_rate_by_period() bins by quarter", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = as.Date(c("2025-02-01", "2025-05-01")),
    counts_toward_rate = c(TRUE, TRUE)
  ))
  rate_df <- peritonitis_rate_by_period(x, cal.period = "quarter")
  expect_equal(nrow(rate_df), 4L)
  expect_equal(rate_df$label, c("Q1 2025", "Q2 2025", "Q3 2025", "Q4 2025"))
  expect_equal(rate_df$n_episodes, c(1L, 1L, 0L, 0L))
})

test_that("peritonitis_rate_by_period() bins by year and clips partial periods to [t0, t1]", {
  x <- make_unit(t0 = as.Date("2024-07-01"), t1 = as.Date("2025-06-30"),
                 tpyar = total_patient_years(list(make_patient(
                   t0 = as.Date("2024-07-01"), t1 = as.Date("2025-06-30")
                 )), as.Date("2024-07-01"), as.Date("2025-06-30")),
                 patient_list = list(make_patient(
                   t0 = as.Date("2024-07-01"), t1 = as.Date("2025-06-30")
                 )))
  rate_df <- peritonitis_rate_by_period(x, cal.period = "year")
  expect_equal(nrow(rate_df), 2L)
  expect_equal(rate_df$period_start, as.Date(c("2024-07-01", "2025-01-01")))
  expect_equal(rate_df$period_end, as.Date(c("2024-12-31", "2025-06-30")))
})

test_that("peritonitis_rate_by_period() gives NA rate when patient_years is 0", {
  x <- new_pd_unit(
    unit_id = "Empty Unit",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-02-28"),
    n_new = 0L, n_patients = 0L, tpyar = 0
  )
  rate_df <- peritonitis_rate_by_period(x, cal.period = "month")
  expect_true(all(is.na(rate_df$rate)))
  expect_equal(rate_df$n_episodes, c(0L, 0L))
})

test_that("monthly_peritonitis_rate() matches peritonitis_rate_by_period(x, 'month')", {
  x <- make_unit()
  expect_equal(monthly_peritonitis_rate(x)$rate,
               peritonitis_rate_by_period(x, cal.period = "month")$rate)
})


# plot.pd_unit()

test_that("plot.pd_unit() returns a ggplot object for each type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  x <- make_unit()
  invisible(utils::capture.output({
    p_month <- plot(x, type = "monthly")
    p_qtr   <- plot(x, type = "quarterly")
    p_year  <- plot(x, type = "yearly")
  }))
  expect_s3_class(p_month, "ggplot")
  expect_s3_class(p_qtr, "ggplot")
  expect_s3_class(p_year, "ggplot")
  # type = "yearly" is the combined two-panel figure, not a single-panel plot
  expect_s3_class(p_year, "patchwork")
})

test_that("plot.pd_unit() rejects an unknown type", {
  skip_if_not_installed("ggplot2")
  x <- make_unit()
  expect_error(plot(x, type = "weekly"))
})

test_that("plot.pd_unit() defaults its benchmark line to x$rate_benchmark", {
  skip_if_not_installed("ggplot2")
  x <- make_unit(rate_benchmark = 0.25)
  p_default <- plot(x, type = "monthly")
  hline_layer <- get_hline_layer(p_default)
  expect_equal(hline_layer$data$yintercept, 0.25)
})

test_that("plot.pd_unit() lets a per-call benchmark override x$rate_benchmark", {
  skip_if_not_installed("ggplot2")
  x <- make_unit(rate_benchmark = 0.25)
  p_override <- plot(x, type = "monthly", benchmark = 0.60)
  hline_layer <- get_hline_layer(p_override)
  expect_equal(hline_layer$data$yintercept, 0.60)
})

test_that("plot.pd_unit() type = 'yearly' returns a combined two-panel figure", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  p <- plot(make_unit(), type = "yearly")
  expect_s3_class(p, "patchwork")
})

test_that("cumulative rate (by hand from the monthly bins) matches summarise_rate()'s overall rate", {
  # pure data-logic check: no plotting/rendering involved, so no ggplot2/
  # patchwork dependency needed here. This mirrors plot_cumulative_rate()'s
  # rate-panel formula: cumsum(n_episodes) / cumsum(patient_years), the
  # actual year-to-date rate in episodes-per-patient-year units -- the
  # same units as the benchmark line, not a dimensionless proportion.
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = as.Date(c("2025-02-10", "2025-07-05")),
    counts_toward_rate = c(TRUE, TRUE)
  ))
  monthly <- peritonitis_rate_by_period(x, cal.period = "month")
  cum_episodes <- cumsum(monthly$n_episodes)
  cum_patient_years <- cumsum(monthly$patient_years)
  # cumulative episode count must only ever increase (or hold) month to month
  expect_true(all(diff(cum_episodes) >= 0))
  expect_equal(cum_episodes[12], 2L)
  # the final month's cumulative rate must equal the whole-period rate that
  # summarise_rate() (the same helper summary.pd_unit() uses) reports --
  # patient-years accrue additively over non-overlapping months, so this
  # is exact, not an approximation
  cum_rate_final <- cum_episodes[12] / cum_patient_years[12]
  overall <- summarise_rate(x)
  expect_equal(cum_rate_final, overall$rate)
})

test_that("plot.pd_unit() type = 'yearly' handles an empty cohort without erroring", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  x <- new_pd_unit(
    unit_id = "Empty Unit",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    n_new = 0L, n_patients = 0L, tpyar = 0
  )
  expect_no_error(p <- plot(x, type = "yearly"))
  expect_s3_class(p, "ggplot")
})


# cumulative_pf_by_month()

test_that("cumulative_pf_by_month() is 100 throughout when no patient ever has an episode", {
  x <- make_unit()
  pf_monthly <- cumulative_pf_by_month(x)
  expect_equal(nrow(pf_monthly), 12L)
  expect_true(all(pf_monthly$cum_pf_pct == 100))
})

test_that("cumulative_pf_by_month() drops exactly when a patient's first episode occurs", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = as.Date("2025-05-10"),
    counts_toward_rate = TRUE
  ))
  pf_monthly <- cumulative_pf_by_month(x)
  # single-patient cohort: PF is 100% through April, 0% from May onward
  expect_equal(pf_monthly$cum_pf_pct[1:4], rep(100, 4))
  expect_equal(pf_monthly$cum_pf_pct[5:12], rep(0, 8))
  # non-increasing across the whole year
  expect_true(all(diff(pf_monthly$cum_pf_pct) <= 0))
})

test_that("cumulative_pf_by_month()'s final value matches summarise_pf()'s overall PF", {
  # give the patients tibble a real n_episodes column (as pd_patient.R does
  # for genuine data) so summarise_pf() can see the same episode that
  # cumulative_pf_by_month() reads off x$infections -- this is the
  # consistency check that matters, not the coincidental has_col()-missing
  # case where both would read 0 regardless of the data
  x <- make_unit(
    patients = tibble::tibble(patient_id = "ABC1234", n_episodes = 1),
    infections = tibble::tibble(
      patient_id = "ABC1234", catheter_id = "ABC1234_01",
      infection_date = as.Date("2025-05-10"),
      counts_toward_rate = TRUE
    )
  )
  pf_monthly <- cumulative_pf_by_month(x)
  overall_pf <- summarise_pf(x)
  # PF drops to 0% once the episode lands in May, and stays there
  expect_equal(pf_monthly$cum_pf_pct[1:4], rep(100, 4))
  expect_equal(pf_monthly$cum_pf_pct[5:12], rep(0, 8))
  expect_equal(pf_monthly$cum_pf_pct[12], overall_pf$pf * 100)
})

test_that("cumulative_pf_by_month() returns NA throughout for an empty cohort", {
  x <- new_pd_unit(
    unit_id = "Empty Unit",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    n_new = 0L, n_patients = 0L, tpyar = 0
  )
  pf_monthly <- cumulative_pf_by_month(x)
  expect_true(all(is.na(pf_monthly$cum_pf_pct)))
})

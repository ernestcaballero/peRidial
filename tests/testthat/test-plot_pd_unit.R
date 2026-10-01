# Shared fixtures (T0, T1, make_catheter(), make_patient(), make_unit()) live
# in test-pd_unit.R and are available here within the same testthat package.

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
  x <- make_unit()
  invisible(utils::capture.output({
    p_month <- plot(x, type = "monthly")
    p_qtr   <- plot(x, type = "quarterly")
    p_year  <- plot(x, type = "yearly")
  }))
  expect_s3_class(p_month, "ggplot")
  expect_s3_class(p_qtr, "ggplot")
  expect_s3_class(p_year, "ggplot")
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
  hline_layer <- p_default$layers[[2]]
  expect_equal(hline_layer$data$yintercept, 0.25)
})

test_that("plot.pd_unit() lets a per-call benchmark override x$rate_benchmark", {
  skip_if_not_installed("ggplot2")
  x <- make_unit(rate_benchmark = 0.25)
  p_override <- plot(x, type = "monthly", benchmark = 0.60)
  hline_layer <- p_override$layers[[2]]
  expect_equal(hline_layer$data$yintercept, 0.60)
})

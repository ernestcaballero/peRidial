# Tests for R/indicators.R. Shared fixtures live in helper-fixtures.R, which testthat sources before every test file.
# peritonitis_rate_by_period() and monthly_peritonitis_rate() are already tested in test-plot_pd_unit.R.

d <- function(x) as.Date(x)

# exposure_days()

test_that("exposure_days() counts both endpoints", {
  expect_equal(exposure_days(d("2025-01-01"), d("2025-12-31")), 365)
  expect_equal(exposure_days(d("2025-03-10"), d("2025-03-19")), 10)
  expect_equal(exposure_days(d("2025-06-15"), d("2025-06-15")), 1)   # a single-day interval is one day at risk, not zero
})

test_that("exposure_days() is zero, never negative, when the interval is empty or reversed", {
  expect_equal(exposure_days(d("2025-06-15"), d("2025-06-14")), 0)   # max_date the day before min_date: nothing at risk
  expect_equal(exposure_days(d("2025-06-15"), d("2024-01-01")), 0)   # well before
})

test_that("exposure_days() is vectorised and returns a plain numeric", {
  out <- exposure_days(d(c("2025-01-01", "2025-02-01", "2025-03-01")),
                       d(c("2025-01-10", "2025-01-15", "2025-03-01")))
  expect_type(out, "double")
  expect_equal(out, c(10, 0, 1))
})

test_that("exposure_days() rejects arguments that are not Dates", {
  expect_error(exposure_days("2025-01-01", d("2025-12-31")))
  expect_error(exposure_days(20250101, 20251231))
})


# catheter_exposure_days()

test_that("catheter_exposure_days() gives the whole period for a catheter still active", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(cath, T0, T1), 365)
})

test_that("catheter_exposure_days() starts counting at pd_start_date when it is inside the period", {
  cath <- list(pd_start_date = d("2025-03-15"), pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(cath, T0, T1), as.numeric(T1 - d("2025-03-15") + 1))
})

test_that("catheter_exposure_days() stops counting at pd_stop_date when it is inside the period", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = d("2025-02-28"))
  expect_equal(catheter_exposure_days(cath, T0, T1), 59)   # 31 Jan + 28 Feb
})

test_that("catheter_exposure_days() clips a window for a catheter active before and after the period", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = d("2026-03-01"))
  expect_equal(catheter_exposure_days(cath, T0, T1), 365)
})

test_that("catheter_exposure_days() counts a catheter that starts and stops inside the period", {
  cath <- list(pd_start_date = d("2025-02-10"), pd_stop_date = d("2025-02-19"))
  expect_equal(catheter_exposure_days(cath, T0, T1), 10)
})

test_that("catheter_exposure_days() is zero for a window wholly outside the period", {
  before <- list(pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31"))
  after <- list(pd_start_date = d("2026-01-01"), pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(before, T0, T1), 0)
  expect_equal(catheter_exposure_days(after, T0, T1), 0)
})

test_that("catheter_exposure_days() counts exactly one day when the window touches the period by a day", {
  stops_on_t0 <- list(pd_start_date = d("2024-01-01"), pd_stop_date = T0)
  starts_on_t1 <- list(pd_start_date = T1, pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(stops_on_t0, T0, T1), 1)
  expect_equal(catheter_exposure_days(starts_on_t1, T0, T1), 1)
})

test_that("catheter_exposure_days() censors an open catheter at tau", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(cath, T0, T1, tau = d("2025-03-31")), 90)  # # left PD on 31 Mar: 1 Jan - 31 Mar 2025 = 90 days
})

test_that("catheter_exposure_days() uses the earliest of pd_stop_date, t1 and tau", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = d("2025-06-30"))
  expect_equal(catheter_exposure_days(cath, T0, T1, tau = d("2025-09-30")), 181) # # tau after the stop date
  expect_equal(catheter_exposure_days(cath, T0, T1, tau = d("2025-01-31")), 31) # tau before the stop date
  open <- list(pd_start_date = d("2024-06-01"), pd_stop_date = as.Date(NA))  # tau after t1
  expect_equal(catheter_exposure_days(open, T0, T1, tau = d("2026-06-01")), 365)
})

test_that("catheter_exposure_days() treats an NA tau as 'never left PD'", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = as.Date(NA))
  expect_equal(catheter_exposure_days(cath, T0, T1, tau = as.Date(NA)),
               catheter_exposure_days(cath, T0, T1))
})

test_that("catheter_exposure_days() rejects non-Date inputs", {
  cath <- list(pd_start_date = d("2024-06-01"), pd_stop_date = as.Date(NA))
  expect_error(catheter_exposure_days(cath, "2025-01-01", T1))
  expect_error(catheter_exposure_days(cath, T0, T1, tau = "2025-03-31"))
})


# total_patient_years()

test_that("total_patient_years() is 0 for an empty cohort", {
  expect_equal(total_patient_years(list(), T0, T1), 0)
})

test_that("total_patient_years() converts exposure days with a 365.25-day year", {
  expect_equal(total_patient_years(list(make_patient()), T0, T1), 365 / 365.25)
})

test_that("total_patient_years() adds patients together", {
  p1 <- make_patient("AAA0001", catheters = list(make_catheter(patient_id = "AAA0001",
                                                                catheter_id = "AAA0001_01")))
  p2 <- make_patient("BBB0002", catheters = list(make_catheter(patient_id = "BBB0002",
                                                                catheter_id = "BBB0002_01")))
  expect_equal(total_patient_years(list(p1, p2), T0, T1), 2 * 365 / 365.25)
})

test_that("total_patient_years() adds a patient's catheters together", {
  c1 <- make_catheter(insertion_date = d("2024-05-01"), pd_start_date = d("2024-06-01"),
                      pd_stop_date = d("2025-03-31"), removal_reason = "mechanical failure")
  c2 <- make_catheter(catheter_id = "ABC1234_02", insertion_date = d("2025-04-01"),
                      pd_start_date = d("2025-04-15"))
  p <- make_patient(catheters = list(c1, c2))    # c1: 90 days (1 Jan - 31 Mar); c2: 261 days (15 Apr - 31 Dec)
  expect_equal(total_patient_years(list(p), T0, T1), (90 + 261) / 365.25)
})

test_that("total_patient_years() censors each patient at their own transfer_date", {
  censored <- make_patient("AAA0001", transfer_date = d("2025-03-31"),
                           catheters = list(make_catheter(patient_id = "AAA0001",
                                                          catheter_id = "AAA0001_01")))
  expect_equal(total_patient_years(list(censored), T0, T1), 90 / 365.25)
})

test_that("total_patient_years() scopes the exposure to the [t0, t1] it is given", {
  p <- make_patient()
  expect_equal(total_patient_years(list(p), d("2025-01-01"), d("2025-03-31")), 90 / 365.25)
  expect_equal(total_patient_years(list(p), d("2025-07-01"), d("2025-07-01")), 1 / 365.25)
})

test_that("total_patient_years() gives 0 for a patient with no catheters", {
  no_cath <- list(catheters = list(), transfer_date = as.Date(NA))
  expect_equal(total_patient_years(list(no_cath), T0, T1), 0)
})

test_that("total_patient_years() treats a patient without a transfer_date field as never censored", {
  bare <- list(catheters = list(make_catheter()))   # no transfer_date element at all
  expect_equal(total_patient_years(list(bare), T0, T1), 365 / 365.25)
})

test_that("total_patient_years() validates its inputs", {
  expect_error(total_patient_years("not a list", T0, T1))
  expect_error(total_patient_years(list(), T0, "2025-12-31"))
  expect_error(total_patient_years(list(), as.Date(NA), T1), "Both t0 and t1")
  expect_error(total_patient_years(list(), T1, T0), "t0 must be on or before t1")
})

test_that("total_patient_years() accepts a single-day period (t0 == t1)", {
  expect_equal(total_patient_years(list(make_patient()), T0, T0), 1 / 365.25)
})


# peritonitis_rate_by_period() / monthly_peritonitis_rate(): cases test-plot_pd_unit.R does not cover

test_that("peritonitis_rate_by_period() computes rate as countable episodes / patient-years", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = d("2025-01-20"), counts_toward_rate = TRUE
  ))
  out <- peritonitis_rate_by_period(x, cal.period = "month")
  jan <- out[out$period == d("2025-01-01"), ]
  expect_equal(jan$patient_years, 31 / 365.25)
  expect_equal(jan$rate, 1 / (31 / 365.25))
  # a month with no episodes has a rate of 0, not NA, while there is time at risk
  expect_equal(out$rate[out$period == d("2025-02-01")], 0)
})

test_that("peritonitis_rate_by_period() leaves out episodes that do not count toward the rate", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    infection_date = d(c("2025-03-10", "2025-03-20")),
    counts_toward_rate = c(TRUE, FALSE)    # the second is e.g. a relapse
  ))
  out <- peritonitis_rate_by_period(x, cal.period = "month")
  expect_equal(out$n_episodes[out$period == d("2025-03-01")], 1L)
  expect_equal(sum(out$n_episodes), 1L)
})

test_that("peritonitis_rate_by_period() puts episodes on a period boundary in the right bin", {
  x <- make_unit(infections = tibble::tibble(
    patient_id = "ABC1234", catheter_id = c("ABC1234_01", "ABC1234_01"),
    infection_date = d(c("2025-03-31", "2025-04-01")), counts_toward_rate = c(TRUE, TRUE)
  ))
  out <- peritonitis_rate_by_period(x, cal.period = "month")
  expect_equal(out$n_episodes[out$period == d("2025-03-01")], 1L)
  expect_equal(out$n_episodes[out$period == d("2025-04-01")], 1L)
})

test_that("peritonitis_rate_by_period() starts at the period containing a mid-month t0", {
  pl <- list(make_patient(t0 = d("2025-01-15"), t1 = d("2025-03-10")))
  x <- make_unit(t0 = d("2025-01-15"), t1 = d("2025-03-10"), patient_list = pl,
                 tpyar = total_patient_years(pl, d("2025-01-15"), d("2025-03-10")))
  out <- peritonitis_rate_by_period(x, cal.period = "month")
  expect_equal(out$period, d(c("2025-01-01", "2025-02-01", "2025-03-01")))   # bin start is the 1st
  expect_equal(out$period_start, d(c("2025-01-15", "2025-02-01", "2025-03-01")))
  expect_equal(out$period_end, d(c("2025-01-31", "2025-02-28", "2025-03-10")))
})


test_that("peritonitis_rate_by_period() patient-years across the bins add up to total_patient_years()", {
  x <- make_unit()
  for (per in c("month", "quarter", "year")) {
    out <- peritonitis_rate_by_period(x, cal.period = per)
    expect_equal(sum(out$patient_years), total_patient_years(x$patient_list, T0, T1))
  }
})

test_that("peritonitis_rate_by_period() rejects bad input", {
  expect_error(peritonitis_rate_by_period(make_unit(), cal.period = "week"))
})

test_that("peritonitis_rate_by_period() errors when the unit has no t0 or t1", {
  x <- make_unit()
  x$t1 <- as.Date(NA)
  expect_error(peritonitis_rate_by_period(x), "Both t0 and t1")
})

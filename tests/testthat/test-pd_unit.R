# Helpers: T0, T1, make_catheter(), make_patient(), make_unit() now live in
# helper-fixtures.R, which testthat sources before every test file (so
# they're available here and in test-plot_pd_unit.R even when a single
# file is run on its own, not just via the full devtools::test() suite).


# Constructor

test_that("new_pd_unit() returns an object of class pd_unit with all fields", {
  x <- make_unit()
  expect_s3_class(x, "pd_unit")
  expect_named(x, c("unit_id", "t0", "t1", "n_new", "n_patients", "tpyar",
                    "rate_benchmark", "patients", "catheters", "infections",
                    "patient_list"))
})

test_that("new_pd_unit() defaults rate_benchmark to 0.40", {
  x <- make_unit()
  expect_equal(x$rate_benchmark, 0.40)
})

test_that("new_pd_unit() accepts a custom rate_benchmark", {
  x <- make_unit(rate_benchmark = 0.30)
  expect_equal(x$rate_benchmark, 0.30)
})

test_that("validate_pd_unit() rejects a missing or negative rate_benchmark", {
  expect_error(validate_pd_unit(make_unit(rate_benchmark = NA_real_)),
               "rate_benchmark is missing")
  expect_error(validate_pd_unit(make_unit(rate_benchmark = -0.1)),
               "rate_benchmark cannot be negative")
})

test_that("new_pd_unit() defaults are empty rather than absent", {
  x <- new_pd_unit()
  expect_true(is.na(x$unit_id))
  expect_true(is.na(x$t0) && is.na(x$t1))
  expect_equal(nrow(x$patients), 0L)
  expect_length(x$patient_list, 0L)
})

test_that("new_pd_unit() rejects malformed scalar arguments", {
  expect_error(new_pd_unit(unit_id = c("A", "B")))
  expect_error(new_pd_unit(t0 = "2025-01-01"))
  expect_error(new_pd_unit(t1 = "2025-12-31"))
  expect_error(new_pd_unit(patients = list()))
  expect_error(new_pd_unit(patient_list = "not a list"))
})


# Reporting period

test_that("validate_pd_unit() requires both t0 and t1", {
  expect_error(validate_pd_unit(make_unit(t0 = as.Date(NA))),
               "Both t0 and t1 must be supplied")
  expect_error(validate_pd_unit(make_unit(t1 = as.Date(NA))),
               "Both t0 and t1 must be supplied")
})

test_that("validate_pd_unit() rejects a period that runs backwards", {
  x <- make_unit(t0 = T1, t1 = T0)
  expect_error(validate_pd_unit(x), "t0 must be on or before t1")
})


# Headline counts

test_that("validate_pd_unit() requires n_new and n_patients", {
  expect_error(validate_pd_unit(make_unit(n_new = NA_integer_)),
               "Both n_new and n_patients must be supplied")
  expect_error(validate_pd_unit(make_unit(n_patients = NA_integer_)),
               "Both n_new and n_patients must be supplied")
})

test_that("validate_pd_unit() rejects negative counts", {
  expect_error(validate_pd_unit(make_unit(n_new = -1L)), "cannot be negative")
})

test_that("validate_pd_unit() rejects more incident patients than patients", {
  x <- make_unit(n_new = 5L, n_patients = 2L)
  expect_error(validate_pd_unit(x), "n_new cannot exceed n_patients")
})

test_that("validate_pd_unit() requires a non-negative tpyar", {
  expect_error(validate_pd_unit(make_unit(tpyar = NA_real_)),
               "Total patient-years-at-risk \\(tpyar\\) is missing")
  expect_error(validate_pd_unit(make_unit(tpyar = -1)),
               "cannot be negative")
})

test_that("validate_pd_unit() rejects tpyar larger than the period allows", {
  # one patient cannot accrue 99 patient-years in a single calendar year
  x <- make_unit(tpyar = 99)
  expect_error(validate_pd_unit(x), "exceeds the maximum possible")
})

test_that("validate_pd_unit() rejects n_new inconsistent with new_patient_flag", {
  # the single patient is prevalent, so n_new must be 0
  x <- make_unit(n_new = 1L)
  expect_error(validate_pd_unit(x), "does not match the number of patients")
})

test_that("an incident patient is counted in n_new", {
  p <- make_patient(catheters = list(make_catheter(
    insertion_date = as.Date("2025-03-01"),
    pd_start_date = as.Date("2025-03-15")
  )))
  expect_true(p$new_patient_flag)
  x <- make_unit(n_new = 1L, patient_list = list(p),
                 tpyar = total_patient_years(list(p), T0, T1))
  expect_s3_class(validate_pd_unit(x), "pd_unit")
})


# Tibbles

test_that("validate_pd_unit() requires nrow(patients) to match n_patients", {
  x <- make_unit(patients = tibble::tibble(
    patient_id = c("ABC1234", "XYZ9999")))
  expect_error(validate_pd_unit(x), "does not match n_patients")
})

test_that("validate_pd_unit() rejects duplicate patient_id in patients", {
  p <- make_patient()
  x <- make_unit(n_patients = 2L,
                 patients = tibble::tibble(patient_id = c("ABC1234", "ABC1234")),
                 patient_list = list(p, p))
  expect_error(validate_pd_unit(x), "Duplicate patient_id")
})

test_that("validate_pd_unit() rejects a catheter with an unknown patient_id", {
  x <- make_unit(catheters = tibble::tibble(patient_id = "NOPE0000",
                                            catheter_id = "NOPE0000_01"))
  expect_error(validate_pd_unit(x), "not present in `patients`")
})

test_that("validate_pd_unit() rejects duplicate catheter_id across the unit", {
  x <- make_unit(catheters = tibble::tibble(
    patient_id = c("ABC1234", "ABC1234"),
    catheter_id = c("ABC1234_01", "ABC1234_01")))
  expect_error(validate_pd_unit(x), "Duplicate catheter_id")
})

test_that("validate_pd_unit() rejects an infection with an unknown catheter_id", {
  x <- make_unit(infections = tibble::tibble(patient_id = "ABC1234",
                                             catheter_id = "NOPE0000_01"))
  expect_error(validate_pd_unit(x), "catheter_id not present in `catheters`")
})

test_that("validate_pd_unit() requires infections to carry a catheter_id column", {
  # a silently-missing column must not pass as vacuously valid
  x <- make_unit(infections = tibble::tibble(patient_id = "ABC1234"))
  expect_error(validate_pd_unit(x), "missing required column")
})


# Nested object list

test_that("validate_pd_unit() requires patient_list length to match n_patients", {
  x <- make_unit(n_patients = 2L,
                 patients = tibble::tibble(patient_id = c("ABC1234", "XYZ9999")))
  expect_error(validate_pd_unit(x), "does not match n_patients")
})

test_that("validate_pd_unit() rejects a non-pd_patient in patient_list", {
  x <- make_unit(patient_list = list(list(patient_id = "ABC1234")))
  expect_error(validate_pd_unit(x), "is not a pd_patient object")
})

test_that("validate_pd_unit() rejects a patient scoped to a different window", {
  p <- make_patient(t0 = as.Date("2024-01-01"), t1 = as.Date("2024-12-31"))
  x <- make_unit(patient_list = list(p), tpyar = 0)
  expect_error(validate_pd_unit(x), "reporting window that differs")
})

test_that("validate_pd_unit() rejects patients tibble disagreeing with patient_list", {
  x <- make_unit(patients = tibble::tibble(patient_id = "SOMEONE_ELSE"))
  expect_error(validate_pd_unit(x), "do not match those in")
})



test_that("is_incident_patient returns TRUE when earliest pd_start_date falls in the window", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-02-15"),
    pd_start_date = as.Date("2025-03-01")
  )
  expect_true(is_incident_patient(list(cath), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient returns FALSE when the patient started PD before the window (prevalent)", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2020-01-01"),
    pd_start_date = as.Date("2020-01-15")
  )
  expect_false(is_incident_patient(list(cath), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient uses the earliest start across multiple catheters, not the latest", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2020-01-01"),
    pd_start_date = as.Date("2020-01-15"),
    pd_stop_date = as.Date("2024-06-01"),
    removal_reason = "mechanical failure"
  )
  second_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH002",
    insertion_date = as.Date("2025-06-01"),
    pd_start_date = as.Date("2025-06-15")
  )
  # second_cath's own start date falls inside the window, but the patient's
  # very first-ever PD start (on first_cath) was years earlier -- prevalent, not incident
  expect_false(is_incident_patient(
    list(first_cath, second_cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  ))
})


test_that("is_incident_patient returns TRUE when the earliest of several catheters falls in the window", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-02-01"),
    pd_start_date = as.Date("2025-02-15")
  )
  later_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH002",
    insertion_date = as.Date("2025-08-01"),
    pd_start_date = as.Date("2025-08-15")
  )
  expect_true(is_incident_patient(
    list(later_cath, first_cath),  # order shouldn't matter
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  ))
})


test_that("is_incident_patient treats window boundaries as inclusive", {
  cath_on_t0 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-01")
  )
  expect_true(is_incident_patient(list(cath_on_t0), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))

  cath_on_t1 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-12-31"),
    pd_start_date = as.Date("2025-12-31")
  )
  expect_true(is_incident_patient(list(cath_on_t1), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient supports an open-ended window (only t0 or only t1 supplied)", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-06-01"),
    pd_start_date = as.Date("2025-06-15")
  )
  # only a floor: on/after t0
  expect_true(is_incident_patient(list(cath), t0 = as.Date("2025-01-01"), t1 = as.Date(NA)))
  expect_false(is_incident_patient(list(cath), t0 = as.Date("2025-07-01"), t1 = as.Date(NA)))

  # only a ceiling: on/before t1
  expect_true(is_incident_patient(list(cath), t0 = as.Date(NA), t1 = as.Date("2025-12-31")))
  expect_false(is_incident_patient(list(cath), t0 = as.Date(NA), t1 = as.Date("2025-05-01")))
})


test_that("is_incident_patient returns NA when neither t0 nor t1 is supplied", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-06-01"),
    pd_start_date = as.Date("2025-06-15")
  )
  expect_true(is.na(is_incident_patient(list(cath), t0 = as.Date(NA), t1 = as.Date(NA))))
})


test_that("is_incident_patient returns NA for an empty catheter list", {
  expect_true(is.na(is_incident_patient(list(), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))))
})


test_that("is_incident_patient returns NA when no catheter has a usable pd_start_date", {
  # built via new_pd_catheter() directly (bypassing validate_pd_catheter(), which
  # would otherwise require pd_start_date) to exercise the helper's own defensiveness
  cath_no_start <- new_pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date(NA)
  )
  expect_true(is.na(is_incident_patient(
    list(cath_no_start), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  )))
})


test_that("is_incident_patient ignores non-pd_catheter elements rather than erroring", {
  not_a_catheter <- list(pd_start_date = as.Date("2025-03-01"))
  expect_true(is.na(is_incident_patient(
    list(not_a_catheter), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  )))
})


test_that("new_pd_patient() derives new_patient_flag from is_incident_patient by default", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2025-02-01"),
    pd_start_date = as.Date("2025-02-15")
  )
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  )
  expect_true(p$new_patient_flag)
})


test_that("validate_pd_patient rejects a new_patient_flag inconsistent with the catheters", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "CATH001",
    insertion_date = as.Date("2020-01-01"),
    pd_start_date = as.Date("2020-01-15")   # well before the window -- this patient is prevalent
  )
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    new_patient_flag = TRUE   # deliberately wrong
  )
  expect_error(validate_pd_patient(p), "new_patient_flag")
})

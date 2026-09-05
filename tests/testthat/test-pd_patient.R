
test_that("new_pd_patient creates a valid pd_patient object", {
  cath <- pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2024-12-15"),
    pd_start_date = as.Date("2025-01-10"),    # inside [t0, t1] -> incident/new patient
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    transfer_reason = NA_character_,          # still active PD patient
    transfer_date = NA_character_,
    new_patient_flag = TRUE,
    n_catheters = 1,
    n_episodes = 0)
  expect_s3_class(validate_pd_patient(p), "pd_patient")
})



test_that("validate_pd_patient errors when patient_id is missing", {
  p <- new_pd_patient(patient_id = NA_character_)   # patient_id genuinely missing
  expect_error(validate_pd_patient(p), "patient_id must be supplied.")
})



test_that("validate_pd_patient errors when t0 is after t1", {
  p <- new_pd_patient(
    patient_id = "ABC1234",
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2024-11-30"))             # t1 is before t0 (and a valid calendar date)
  expect_error(validate_pd_patient(p), "t0 must be on or before t1.")
})




test_that("new_pd_patient() allows an active patient with transfer_reason and transfer_date both NA", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-02-01"),
    pd_start_date = as.Date("2025-02-15"),
    t0 = as.Date("2025-01-01"),               # must match the patient's window
    t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_true(is.na(p$transfer_reason))
  expect_true(is.na(p$transfer_date))
  expect_s3_class(p$transfer_date, "Date")
  expect_silent(validate_pd_patient(p))
})



test_that("new_pd_patient() coerces a bare NA transfer_date into a Date NA", {
  p <- new_pd_patient(
    patient_id = "ABC1234",
    transfer_date = NA)        # logical NA, not as.Date(NA)
  expect_s3_class(p$transfer_date, "Date")
  expect_true(is.na(p$transfer_date))
})



test_that("validate_pd_patient errors when transfer_date is supplied without transfer_reason", {
  p <- new_pd_patient(
    patient_id = "ABC1234",
    transfer_date = as.Date("2025-06-01"))   # no transfer_reason to go with it
  expect_error(validate_pd_patient(p), "transfer_reason")
})



test_that("validate_pd_patient errors when transfer_reason is supplied without transfer_date", {
  p <- new_pd_patient(
    patient_id = "ABC1234",
    transfer_reason = "death")   # no transfer_date to go with it
  expect_error(validate_pd_patient(p), "date of transfer")
})




test_that("is_incident_patient returns TRUE when earliest pd_start_date falls in the window", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-02-15"),
    pd_start_date = as.Date("2025-03-01"))
  expect_true(is_incident_patient(list(cath), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient returns FALSE when the patient started PD before the window [t0, t1]", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2024-01-01"),
    pd_start_date = as.Date("2024-01-15"))
  expect_false(is_incident_patient(list(cath), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient uses the earliest start across multiple catheters, not the latest, within [t0, t1]", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2024-11-10"),
    pd_start_date = as.Date("2024-11-25"),        # PD start before t0
    pd_stop_date = as.Date("2025-06-01"),
    removal_reason = "mechanical failure")
  second_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_02",
    insertion_date = as.Date("2025-07-01"),
    pd_start_date = as.Date("2025-07-15"))       # PD start within [t0, t1]

  # should be false because first ever catheter was before the reporting period.
  expect_false(is_incident_patient(
    list(first_cath, second_cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient returns TRUE when the earliest of several catheters falls in the window", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-02-01"),
    pd_start_date = as.Date("2025-02-15"))
  second_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_02",
    insertion_date = as.Date("2025-08-01"),
    pd_start_date = as.Date("2025-08-15"))
  expect_true(is_incident_patient(
    list(second_cath, first_cath),  # order shouldn't matter
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  ))
})


test_that("is_incident_patient treats window boundaries as inclusive", {
  cath_on_t0 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-01"))        # falls exactly on t0
  expect_true(is_incident_patient(list(cath_on_t0), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))

  cath_on_t1 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_02",
    insertion_date = as.Date("2025-12-31"),
    pd_start_date = as.Date("2025-12-31"))        # falls exactly on t1
  expect_true(is_incident_patient(list(cath_on_t1), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")))
})


test_that("is_incident_patient returns NA for an empty catheter list", {
  expect_true(is.na(is_incident_patient(list(), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))))
})


test_that("is_incident_patient returns NA when no catheter has a usable pd_start_date", {
  # built via new_pd_catheter() directly
  cath_no_start <- new_pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date(NA))
  expect_true(is.na(is_incident_patient(
    list(cath_no_start), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))))
})


test_that("is_incident_patient ignores non-pd_catheter elements rather than erroring", {
  not_a_catheter <- list(pd_start_date = as.Date("2025-03-01"))
  expect_true(is.na(is_incident_patient(list(not_a_catheter), t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))))
})



test_that("new_pd_patient() derives new_patient_flag from is_incident_patient by default", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-02-01"),
    pd_start_date = as.Date("2025-02-15"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_true(p$new_patient_flag)
})



test_that("validate_pd_patient rejects a new_patient_flag inconsistent with the catheters", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2023-01-01"),
    pd_start_date = as.Date("2023-01-15"),  # well before the window = this patient is not incident or new to PD
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))  # must match the patient's window
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    new_patient_flag = TRUE)   # deliberately wrong
  expect_error(validate_pd_patient(p), "new_patient_flag")
})


test_that("validate_pd_patient rejects a catheter whose patient_id doesn't match the patient's", {
  cath <- pd_catheter(
    patient_id = "XYZ9999", catheter_id = "XYZ9999_01",   # different patient_id than the patient
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "does not match")
})



test_that("validate_pd_patient errors when a catheters element is not a pd_catheter object", {
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list("not a catheter"),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "is not a pd_catheter object")
})



test_that("validate_pd_patient errors when a nested catheter has a missing pd_start_date", {
  # built via new_pd_catheter() directly
  cath_no_start <- new_pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date(NA),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath_no_start),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "missing pd_start_date")
})



test_that("validate_pd_patient errors when a nested catheter has no pd_stop_date element", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    pd_stop_date = as.Date(NA),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  cath$pd_stop_date <- NULL   # malformed catheter missing
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "must be present")
})



test_that("validate_pd_patient errors when a catheter's pd_start_date is after the patient's transfer_date", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-07-01"),
    pd_start_date = as.Date("2025-07-10"),   # starts after the patient's transfer_date (tau)
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    transfer_reason = "death",
    transfer_date = as.Date("2025-06-01"))   # before the catheter even started
  expect_error(validate_pd_patient(p), "pd_start_date after this patient's")
})



test_that("validate_pd_patient errors when a catheter is still active but the patient has already transferred out", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),   # started before tau, never closed (still active)
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    transfer_reason = "transplant",
    transfer_date = as.Date("2025-06-01"))
  expect_error(validate_pd_patient(p), "still active")
})



test_that("validate_pd_patient errors when a catheter's pd_stop_date is after the patient's transfer_date", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-10"),
    pd_start_date = as.Date("2025-01-25"),
    pd_stop_date = as.Date("2025-07-01"),    # stops after the patient's transfer_date (tau)
    removal_reason = "mechanical failure",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    transfer_reason = "death",
    transfer_date = as.Date("2025-06-01"))
  expect_error(validate_pd_patient(p), "pd_stop_date after this patient's transfer_date.")
})



test_that("validate_pd_patient errors when a catheter's reporting window differs from the patient's", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    t0 = as.Date("2024-01-01"), t1 = as.Date("2024-12-31"))   # different window than the patient's
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "reporting window that differs")
})



test_that("validate_pd_patient errors on duplicate catheter_id within a patient", {
  cath1 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    pd_stop_date = as.Date("2025-03-01"),
    removal_reason = "mechanical failure",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  cath2 <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",   # same catheter_id as cath1
    insertion_date = as.Date("2025-04-01"),
    pd_start_date = as.Date("2025-04-10"),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath1, cath2),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "Duplicate catheter_id")
})



test_that("validate_pd_patient errors when an open-ended catheter is followed by another", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),   # no pd_stop_date, which means it is still "active"
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  second_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_02",
    insertion_date = as.Date("2025-06-01"),
    pd_start_date = as.Date("2025-06-10"),   # starts after first_cath, which never closed
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(first_cath, second_cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "Catheter intervals within a patient must not overlap.")
})



test_that("validate_pd_patient errors on overlapping catheter intervals", {
  first_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    pd_stop_date = as.Date("2025-07-01"),
    removal_reason = "mechanical failure",
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  second_cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_02",
    insertion_date = as.Date("2025-06-01"),
    pd_start_date = as.Date("2025-06-10"),   # starts before first_cath's pd_stop_date, overlaps
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(first_cath, second_cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_patient(p), "overlapping PD intervals")
})



test_that("validate_pd_patient errors when n_catheters does not match length(catheters)", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  )
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    n_catheters = 2   # wrong, only 1 catheter supplied
  )
  expect_error(validate_pd_patient(p), "n_catheters does not match")
})



test_that("validate_pd_patient errors when n_episodes does not match the total episode count", {
  cath <- pd_catheter(
    patient_id = "ABC1234", catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-01"),
    pd_start_date = as.Date("2025-01-10"),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31")
  )
  p <- new_pd_patient(
    patient_id = "ABC1234",
    catheters = list(cath),
    t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
    n_episodes = 3   # deliberately wrong -- cath has no recorded infections
  )
  expect_error(validate_pd_patient(p), "n_episodes does not match")
})

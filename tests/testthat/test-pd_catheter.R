
test_that("new_pd_catheter creates a valid pd_catheter object", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("25-01-2025"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("10-02-2025"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("01-01-2025"),
    t1 = as.Date("31-12-2025"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_s3_class(cath, "pd_catheter")
})


test_that("validate_pd_catheter passes on a well-formed object", {
  cath <- new_pd_infection(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    infection_date = as.Date("15-03-2025"),
    organism_list = list("negative"),
    last_dose_antibiotic = as.Date("29-03-2025")
  )
  expect_identical(validate_pd_infection(cath), cath)
})


test_that("new_pd_catheter creates a valid pd_catheter object", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_s3_class(cath, "pd_catheter")
})



test_that("validate_pd_catheter errors when patient_id or catheter_id is missing", {
  cath <- new_pd_catheter(
    patient_id = NA_character_,
    catheter_id = NA_character_,
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "Both patient_id and catheter_id must be supplied.")
})



test_that("validate_pd_catheter errors when insertion_date is missing", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date(NA),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "Missing insertion_date. Must be supplied.")
})



test_that("validate_pd_catheter errors when pd_start_date is missing", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date(NA),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "Missing pd_start_date. Must be supplied.")
})




test_that("validate_pd_catheter errors when insertion_date after pd_start_date", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-02-11"),     # insertion_date is after PD start
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "insertion_date must be on or before pd_start_date.")
})




test_that("validate_pd_catheter errors when pd_stop_date is prior to pd_start_date", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date("2025-02-01"),        # stop date is before PD start
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "pd_start_date must be on or before pd_stop_date.")
})



test_that("validate_pd_catheter errors when pd_stop_date is empty when a reason for catheter removal is supplied", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = "transferred to HD permanently",
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 100,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "pd_stop_date must be supplied when removal_reason is given.")
})




test_that("validate_pd_catheter errors exposure days is non-positive", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = -1,
    n_peritonitis_episodes = 2,
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath), "total_exposure_days cannot be negative.")
})




test_that("validate_pd_catheter errors when total_exposure_days exceeds the pd_start/pd_stop span", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date("2025-02-10"),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 400,
    n_peritonitis_episodes = 0,
    peritonitis_flag = FALSE)
  expect_error(validate_pd_catheter(cath), "total_exposure_days cannot exceed the span between pd_start_date and pd_stop_date.")
})



test_that("validate_pd_catheter errors when total_exposure_days exceeds pd_start_date to t1 for a still-active catheter", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    total_exposure_days = 400,
    n_peritonitis_episodes = 0,
    peritonitis_flag = FALSE)
  expect_error(validate_pd_catheter(cath), "total_exposure_days cannot exceed the span between pd_start_date and the reporting period's end \\(t1\\), for a still-active catheter with no pd_stop_date.")
})




# Checks for helper count_episodes_in_period
test_that("count_episodes_in_period uses pd_stop_date as the upper bound instead of t1 when supplied", {
  make_inf <- function(infection_date, last_dose_antibiotic) {
    pd_infection(patient_id = "ABC1234", infection_date = as.Date(infection_date),
                organism_list = list("E. coli"),
                last_dose_antibiotic = as.Date(last_dose_antibiotic))
  }

  infections <- list(
    make_inf("2025-09-15", "2025-09-29"),  # before pd_start_date = excluded
    make_inf("2025-10-15", "2025-10-29"),  # inside [pd_start_date, pd_stop_date] = counted
    make_inf("2025-11-15", "2025-11-29")   # after pd_stop_date, though still inside [t0, t1] = excluded
  )
  n <- count_episodes_in_period(infections,
                                t0 = as.Date("2025-01-01"),
                                t1 = as.Date("2025-12-31"),
                                pd_start_date = as.Date("2025-10-02"),
                                pd_stop_date = as.Date("2025-10-31"))
  expect_identical(n, 1L)
})

test_that("count_episodes_in_period falls back to t1 as the upper bound for a still-active catheter (pd_stop_date NA)", {
  make_inf <- function(infection_date, last_dose_antibiotic) {
    pd_infection(patient_id = "ABC1234", infection_date = as.Date(infection_date),
                organism_list = list("E. coli"),
                last_dose_antibiotic = as.Date(last_dose_antibiotic))
  }

  infections <- list(
    make_inf("2025-10-15", "2025-10-29"),  # inside [pd_start_date, t1] = counted
    make_inf("2026-01-15", "2026-01-29")   # after t1, and no pd_stop_date to bound it either = excluded
  )
  n <- count_episodes_in_period(infections,
                                t0 = as.Date("2025-01-01"),
                                t1 = as.Date("2025-12-31"),
                                pd_start_date = as.Date("2025-10-02"),
                                pd_stop_date = as.Date(NA))
  expect_identical(n, 1L)
})


test_that("count_episodes_in_period raises the lower bound to t0 when pd_start_date is before the reporting period", {
  make_inf <- function(infection_date, last_dose_antibiotic) {
    pd_infection(patient_id = "ABC1234", infection_date = as.Date(infection_date),
                organism_list = list("E. coli"),
                last_dose_antibiotic = as.Date(last_dose_antibiotic))
  }

  infections <- list(
    make_inf("2024-06-01", "2024-06-15"),  # before both pd_start_date and t0 = excluded
    make_inf("2024-11-01", "2024-11-15"),  # after pd_start_date, but before t0 = excluded (t0 will be the lower bound)
    make_inf("2025-03-01", "2025-03-15")   # on/after t0 = counted
  )
  n <- count_episodes_in_period(infections,
                                t0 = as.Date("2025-01-01"),
                                t1 = as.Date("2025-12-31"),
                                pd_start_date = as.Date("2024-10-01"),
                                pd_stop_date = as.Date(NA))
  expect_identical(n, 1L)
})


test_that("validate_pd_catheter errors when infections contains a non-pd_infection object", {
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list("not an infection object"),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_catheter(cath), "infections\\[\\[1\\]\\] is not a pd_infection object.")
})


test_that("validate_pd_catheter errors when an infection's patient_id does not match the catheter's patient_id", {
  infxn <- pd_infection(patient_id = "WRONG999",
                        infection_date = as.Date("2025-03-01"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-03-15"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_catheter(cath),
              "infections\\[\\[1\\]\\] has patient_id 'WRONG999', which does not match this catheter's patient_id 'ABC1234'.")
})


test_that("validate_pd_catheter errors when an infection has a missing infection_date", {
  infxn <- new_pd_infection(patient_id = "ABC1234",
                            infection_date = as.Date(NA),          # no infection date
                            organism_list = list("E. coli"),
                            last_dose_antibiotic = as.Date("2025-10-29"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_catheter(cath),
              "infections\\[\\[1\\]\\] has a missing infection_date. Must be supplied.")
})


test_that("validate_pd_catheter errors when an infection's infection_date is before this catheter's pd_start_date", {
  infxn <- pd_infection(patient_id = "ABC1234",
                        infection_date = as.Date("2025-01-30"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-02-13"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_catheter(cath),
              "infections\\[\\[1\\]\\] has infection_date before this catheter's pd_start_date.")
})


test_that("validate_pd_catheter errors when an infection's infection_date is after this catheter's pd_stop_date", {
  infxn <- pd_infection(patient_id = "ABC1234",
                        infection_date = as.Date("2025-03-20"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-04-03"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date("2025-03-10"),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_error(validate_pd_catheter(cath),
              "infections\\[\\[1\\]\\] has infection_date after this catheter's pd_stop_date.")
})


test_that("validate_pd_catheter errors when n_peritonitis_episodes does not match the actual infection count", {
  infxn <- pd_infection(patient_id = "ABC1234",
                        infection_date = as.Date("2025-10-15"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-10-29"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    n_peritonitis_episodes = 5,   # actual count is only 1
    peritonitis_flag = TRUE)
  expect_error(validate_pd_catheter(cath),
              "n_peritonitis_episodes does not match the number of infections falling within this catheter's active window and \\[t0, t1\\].")
})


test_that("validate_pd_catheter errors when peritonitis_flag does not match the derived episode count", {
  infxn <- pd_infection(patient_id = "ABC1234",
                        infection_date = as.Date("2025-10-15"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-10-29"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"),
    n_peritonitis_episodes = 1,    # correct
    peritonitis_flag = FALSE)      # wrong = should be TRUE since count > 0
  expect_error(validate_pd_catheter(cath),
              "peritonitis_flag does not match whether any infection falls within this catheter's active window and \\[t0, t1\\].")
})


test_that("validate_pd_catheter passes with a valid nested infection matching the catheter's derived counts", {
  infxn <- pd_infection(patient_id = "ABC1234",
                        infection_date = as.Date("2025-10-15"),
                        organism_list = list("E. coli"),
                        last_dose_antibiotic = as.Date("2025-10-29"))
  cath <- new_pd_catheter(
    patient_id = "ABC1234",
    catheter_id = "ABC1234_01",
    insertion_date = as.Date("2025-01-25"),
    procedure_type = "laparoscopic",
    pd_start_date = as.Date("2025-02-10"),
    pd_stop_date = as.Date(NA),
    removal_reason = NA_character_,
    infections = list(infxn),
    t0 = as.Date("2025-01-01"),
    t1 = as.Date("2025-12-31"))
  expect_identical(validate_pd_catheter(cath), cath)
  expect_identical(cath$n_peritonitis_episodes, 1L)
  expect_true(cath$peritonitis_flag)
})

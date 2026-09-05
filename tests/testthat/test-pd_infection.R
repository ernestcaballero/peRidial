test_that("new_pd_infection creates a valid pd_infection object", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("Staphylococcus aureus"),
    last_dose_antibiotic = as.Date("2025-03-29"),
    outcome = NA_character_,
    outcome_date = as.Date(NA)
  )
  expect_s3_class(x, "pd_infection")
})

test_that("validate_pd_infection errors when patient_id is NA", {
  x <- new_pd_infection(
    patient_id = NA_character_,
    infection_date = as.Date("2025-03-15"),
    organism_list = list("Staphylococcus aureus"),
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_error(validate_pd_infection(x), "patient_id must be supplied")
})


test_that("new_pd_infection errors when infection_date is not a Date", {
  expect_error(
    new_pd_infection(
      patient_id = "ABC1234",
      infection_date = "2025-03-15",   # character, not a Date
      organism_list = list("Staphylococcus aureus"),
      last_dose_antibiotic = as.Date("2025-03-29")
    )
  )
})



test_that("validate_pd_infection rejects 'negative' combined with other organisms", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative", "E. coli"),
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_error(validate_pd_infection(x), "'negative' cannot be combined with other organisms in the organism_list.")
})



test_that("validate_pd_infection rejects an invalid episode_type", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative"),
    episode_type = "initial",   # not one of the three valid categories
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_error(validate_pd_infection(x), "episode_type")
})


test_that("validate_pd_infection rejects outcome_date without an outcome", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative"),
    last_dose_antibiotic = as.Date("2025-03-29"),
    outcome_date = as.Date("2025-04-01")
  )
  expect_error(validate_pd_infection(x), "outcome_date")
})


test_that("validate_pd_infection requires outcome_date when catheter is removed", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("Staphylococcus aureus"),
    last_dose_antibiotic = as.Date("2025-03-29"),
    outcome = "catheter removed"
  )
  expect_error(validate_pd_infection(x), "catheter removal")
})


test_that("pd_infection() classifies a relapsing episode", {
  prior <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-25"),             # 10 days after prior treatment ended
    organism_list = list("E. coli"),                    # can add variation here like organism name spelled out
    last_dose_antibiotic = as.Date("2025-02-08"),
    prior_episode = prior
  )
  expect_equal(x$episode_type, "relapsing")
})


test_that("pd_infection() classifies a recurrent episode", {
  prior <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-25"),          # within 4 weeks
    organism_list = list("Staph aureus"),            # different organism
    last_dose_antibiotic = as.Date("2025-02-08"),
    prior_episode = prior
  )
  expect_equal(x$episode_type, "recurrent")
})



test_that("pd_infection() classifies a repeat episode", {
  prior <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-01"),          # 45 days after prior treatment ended (>4 weeks)
    organism_list = list("E. coli"),                 # same organism
    last_dose_antibiotic = as.Date("2025-03-15"),
    prior_episode = prior
  )
  expect_equal(x$episode_type, "repeat")
})



test_that("pd_infection() returns NA episode_type when >4 weeks later with a different organism", {
  prior <- new_pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234",
    infection_date = as.Date("2025-03-01"),          # 45 days after prior treatment ended (>4 weeks)
    organism_list = list("Staph aureus"),            # different organism
    last_dose_antibiotic = as.Date("2025-03-15"),
    prior_episode = prior
  )
  expect_true(is.na(x$episode_type))
})

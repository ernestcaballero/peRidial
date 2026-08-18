test_that("new_pd_infection creates a valid pd_infection object", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    catheter_id = "CATH001",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("Staphylococcus aureus"),
    last_dose_antibiotic = as.Date("2025-03-29"),
    outcome = "resolved",
    outcome_date = as.Date("2025-03-29")
  )

  expect_s3_class(x, "pd_infection")
  expect_equal(x$patient_id, "ABC1234")
  expect_equal(x$organism_list, list("Staphylococcus aureus"))
})


test_that("validate_pd_infection passes on a well-formed object", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    catheter_id = "CATH001",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative"),
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_identical(validate_pd_infection(x), x)
})


test_that("validate_pd_infection catches missing patient_id/catheter_id", {
  x <- new_pd_infection(
    patient_id = NA_character_,
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative"),
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_error(validate_pd_infection(x), "patient_id and catheter_id")
})


test_that("validate_pd_infection rejects 'negative' combined with other organisms", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    catheter_id = "CATH001",
    infection_date = as.Date("2025-03-15"),
    organism_list = list("negative", "E. coli"),
    last_dose_antibiotic = as.Date("2025-03-29")
  )
  expect_error(validate_pd_infection(x), "negative")
})


test_that("validate_pd_infection rejects an invalid episode_type", {
  x <- new_pd_infection(
    patient_id = "ABC1234",
    catheter_id = "CATH001",
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
    catheter_id = "CATH001",
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
    catheter_id = "CATH001",
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
    catheter_id = "CATH001",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234",
    catheter_id = "CATH001",
    infection_date = as.Date("2025-01-25"),             # 10 days after prior treatment ended
    organism_list = list("Escherichia coli"),           # same organism, spelled out
    last_dose_antibiotic = as.Date("2025-02-08"),
    prior_episode = prior
  )
  expect_equal(x$episode_type, "relapsing")
})


test_that("pd_infection() classifies a recurrent episode", {
  prior <- new_pd_infection(
    patient_id = "ABC1234", catheter_id = "CATH001",
    infection_date = as.Date("2025-01-01"),
    organism_list = list("E. coli"),
    last_dose_antibiotic = as.Date("2025-01-15")
  )
  x <- pd_infection(
    patient_id = "ABC1234", catheter_id = "CATH001",
    infection_date = as.Date("2025-01-25"),          # within 4 weeks
    organism_list = list("Staph aureus"),            # different organism
    last_dose_antibiotic = as.Date("2025-02-08"),
    prior_episode = prior
  )
  expect_equal(x$episode_type, "recurrent")
})

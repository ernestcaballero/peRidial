test_that("returns a character vector the same length as its inputs", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 3),
    infection_date = as.Date(c("2025-01-01", "2025-01-25", "2025-03-01")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08", "2025-03-15")),
    organism_list = list(list("E. coli"), list("E. coli"), list("E. coli"))
  )
  expect_type(x, "character")
  expect_length(x, 3)
  expect_true(is.na(x[1])) # first episode has no prior episode to compare against
})


test_that("classifies a relapsing episode", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),   # 2nd infection 10 days after 1st treatment ended
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("E. coli"), list("E. coli"))     # same organism
  )
  expect_equal(x, c(NA_character_, "relapsing"))
})


test_that("classify_episode_types() classifies a relapsing episode: negative culture followed by a specific organism", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("negative"), list("E. coli"))    # specific organism after a negative culture
  )
  expect_equal(x, c(NA_character_, "relapsing"))
})


test_that("classifies a recurrent episode", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),   # within 4 weeks
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("E. coli"), list("Staph aureus"))  # different organism
  )
  expect_equal(x, c(NA_character_, "recurrent"))
})


test_that("classifies a repeat episode", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-03-01")),   # 45 days after prior treatment ended (>4 weeks)
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-03-15")),
    organism_list = list(list("E. coli"), list("E. coli"))     # same organism
  )
  expect_equal(x, c(NA_character_, "repeat"))
})


test_that("returns NA when >4 weeks later with a different organism", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-03-01")),   # 45 days after prior treatment ended (>4 weeks)
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-03-15")),
    organism_list = list(list("E. coli"), list("Staph aureus"))  # different organism
  )
  expect_true(is.na(x[2]))
  expect_true(all(is.na(x)))                          # neither episode gets a category
})


test_that("matches organisms regardless of case or order", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("E. coli", "Staph aureus"),
                         list("staph aureus", "e. coli"))   # same two organisms, different case and order
  )
  expect_equal(x, c(NA_character_, "relapsing"))
})


test_that("does not compare episodes across different patients", {
  x <- classify_episode_types(
    patient_id = c("P1", "P2"),
    infection_date = as.Date(c("2025-01-01", "2025-01-20")),   # P2's infection is only 5 days after P1's last dose
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-03")),
    organism_list = list(list("E. coli"), list("E. coli"))
  )
  expect_equal(x, c(NA_character_, NA_character_))     # each is that patient's first episode
})


test_that("treats a catheter-removed outcome with no outcome_date as invalid", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("E. coli"), list("E. coli")),
    outcome = c(NA_character_, "catheter removed")   # outcome_date left NA, so validation rejects row 2
  )
  expect_equal(x, c(NA_character_, NA_character_))
})


test_that("treats a missing last_dose_antibiotic as invalid, leaving no prior episode", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),
    last_dose_antibiotic = c(as.Date(NA), as.Date("2025-02-08")),   # row 1 fails validation
    organism_list = list(list("E. coli"), list("E. coli"))
  )
  expect_equal(x, c(NA_character_, NA_character_))
})


test_that("errors when infection_date is not a Date", {
  expect_error(
    classify_episode_types(
      patient_id = "ABC1234",
      infection_date = "2025-01-01",                # character, not a Date
      last_dose_antibiotic = as.Date("2025-01-15"),
      organism_list = list(list("E. coli"))
    ),
    "infection_date"
  )
})


test_that("errors when organism_list is not a list", {
  expect_error(
    classify_episode_types(
      patient_id = "ABC1234",
      infection_date = as.Date("2025-01-01"),
      last_dose_antibiotic = as.Date("2025-01-15"),
      organism_list = "E. coli"                      # a bare string, not a list
    ),
    "organism_list"
  )
})


test_that("errors when the inputs are different lengths", {
  expect_error(
    classify_episode_types(
      patient_id = c("ABC1234", "ABC1234"),
      infection_date = as.Date(c("2025-01-01", "2025-01-25")),
      last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
      organism_list = list(list("E. coli"))          # one element for two episodes
    ),
    "organism_list"
  )
})

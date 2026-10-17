
test_that("classify_episode_types() classifies a relapsing episode:
          negative culture followed by a specific organism", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-31")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-21")),
    organism_list = list(list("negative"), list("E. coli"))    # specific organism after a negative culture
  )
  expect_equal(x, c(NA_character_, "relapsing"))
})

test_that("classify_episode_types() matches organisms regardless of case or order", {
  x <- classify_episode_types(
    patient_id = rep("ABC1234", 2),
    infection_date = as.Date(c("2025-01-01", "2025-01-25")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-08")),
    organism_list = list(list("E. coli", "Staph aureus"),
                         list("staph aureus", "e. coli"))   # same two organisms, different case and order
  )
  expect_equal(x, c(NA_character_, "relapsing"))
})

test_that("classify_episode_types() treats a missing last_dose_antibiotic as invalid, leaving no prior episode", {
  expect_warning(
    x <- classify_episode_types(
      patient_id = rep("ABC1234", 2),
      infection_date = as.Date(c("2025-01-01", "2025-01-25")),
      last_dose_antibiotic = c(as.Date(NA), as.Date("2025-02-08")),   # row 1 fails validation
      organism_list = list(list("E. coli"), list("E. coli"))
      ), "1 of 2 episode\\(s\\) failed validation")
  expect_equal(x, c(NA_character_, NA_character_))
})

test_that("classify_episode_types() errors when infection_date is not a Date", {
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

test_that("classify_episode_types() errors when organism_list is not a list", {
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

test_that("classify_episode_types() errors when the inputs are different lengths", {
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

test_that("classify_episode_types() returns an empty result for no episodes", {
  out <- classify_episode_types(character(0), as.Date(character(0)),
                                as.Date(character(0)), list())
  expect_identical(out, character(0))
})


# 4-week boundary: "within 4 weeks" is 28 days or fewer after the last antibiotic dose

test_that("classify_episode_types() treats day 28 as within 4 weeks and day 29 as outside, in C++ and in R", {
  for (use_cpp in c(TRUE, FALSE)) {
    day28 <- classify_episode_types(
      patient_id = rep("ABC1234", 2),
      infection_date = as.Date(c("2025-01-01", "2025-02-12")),        # 2025-01-15 + 28 days
      last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-26")),
      organism_list = list(list("E. coli"), list("E. coli")),
      use_cpp = use_cpp)
    day29 <- classify_episode_types(
      patient_id = rep("ABC1234", 2),
      infection_date = as.Date(c("2025-01-01", "2025-02-13")),        # 2025-01-15 + 29 days
      last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-27")),
      organism_list = list(list("E. coli"), list("E. coli")),
      use_cpp = use_cpp)
    expect_equal(day28, c(NA_character_, "relapsing"))
    expect_equal(day29, c(NA_character_, "repeat"))
  }
})

test_that("classify_episode_types() puts a different organism on day 28 in recurrent and on day 29 in NA", {
  for (use_cpp in c(TRUE, FALSE)) {
    day28 <- classify_episode_types(
      patient_id = rep("ABC1234", 2),
      infection_date = as.Date(c("2025-01-01", "2025-02-12")),
      last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-26")),
      organism_list = list(list("E. coli"), list("Klebsiella")),
      use_cpp = use_cpp)
    day29 <- classify_episode_types(
      patient_id = rep("ABC1234", 2),
      infection_date = as.Date(c("2025-01-01", "2025-02-13")),
      last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-27")),
      organism_list = list(list("E. coli"), list("Klebsiella")),
      use_cpp = use_cpp)
    expect_equal(day28, c(NA_character_, "recurrent"))
    expect_equal(day29, c(NA_character_, NA_character_))
  }
})

# C++ and pure-R loops must agree

mixed_episodes <- function() {
  list(
    patient_id = c("A", "A", "A", "A", "B", "B", "B", "C", "C"),
    infection_date = as.Date(c("2025-01-01", "2025-01-20", "2025-02-20", "2025-06-01",
                               "2025-02-01", "2025-02-25", "2025-08-01",
                               "2025-03-01", "2025-03-20")),
    last_dose_antibiotic = as.Date(c("2025-01-15", "2025-02-03", "2025-03-05", "2025-06-15",
                                     "2025-02-15", "2025-03-10", "2025-08-15",
                                     NA, "2025-04-03")),                 # C's first row is invalid
    organism_list = list(list("E. coli"), list("E. coli"), list("Klebsiella"), list("Klebsiella"),
                         list("negative"), list("E. coli"), list("Staphylococcus aureus"),
                         list("E. coli"), list("E. coli"))
  )
}

test_that("classify_episode_types() gives the same episode type with use_cpp = TRUE and use_cpp = FALSE", {
  x <- mixed_episodes()
  expected <- c(NA, "relapsing", "recurrent", "repeat",    # A
                NA, "relapsing", NA,                       # B: negative then specific = relapsing; new organism > 4 weeks = NA
                NA, NA)                                    # C: row 1 invalid, row 2 has no valid prior
  cpp <- suppressWarnings(do.call(classify_episode_types, c(x, use_cpp = TRUE)))
  r <- suppressWarnings(do.call(classify_episode_types, c(x, use_cpp = FALSE)))
  expect_identical(cpp, r)
  expect_equal(cpp, expected)
})


test_that("an invalid row does not become the prior episode in either mode", {
  # row 2 is invalid (catheter removed, no date), so row 3 is compared with row 1
  for (use_cpp in c(TRUE, FALSE)) {
    expect_warning(
      x <- classify_episode_types(
        patient_id = rep("ABC1234", 3),
        infection_date = as.Date(c("2025-01-01", "2025-01-10", "2025-01-25")),
        last_dose_antibiotic = as.Date(c("2025-01-08", "2025-01-20", "2025-02-08")),
        organism_list = list(list("E. coli"), list("Klebsiella"), list("E. coli")),
        outcome = c(NA, "catheter removed", NA),
        use_cpp = use_cpp),
      "row 2: Missing date of catheter removal")
    expect_equal(x, c(NA_character_, NA_character_, "relapsing"))   # 17 days after row 1's last dose, same organism
  }
})

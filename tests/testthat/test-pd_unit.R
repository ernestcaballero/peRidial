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


# Headline PD unit counts

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




# subset()

test_that("subset() returns a plain tibble, not a pd_unit", {
  y <- subset(make_subset_unit())
  expect_s3_class(y, "tbl_df")
  expect_false(inherits(y, "pd_unit"))
})

test_that("subset() with no arguments returns the whole flat table", {
  x <- make_subset_unit()
  y <- subset(x)

  expect_equal(nrow(y), 6L)
  # every patient and every catheter is present, including the catheter with no episodes
  expect_identical(sort(unique(y$patient_id)), c("AAA0001", "BBB0002", "CCC0003"))
  expect_equal(sort(unique(y$catheter_id)), sort(x$catheters$catheter_id))

  # columns from all three levels, with the join keys appearing once
  expect_true(all(c("gender", "ethnicity", "procedure_type",
                    "episode_type", "infection_date") %in% names(y)))
  expect_equal(sum(names(y) == "patient_id"), 1L)
  expect_equal(sum(names(y) == "catheter_id"), 1L)
})

test_that("subset() carries each level's values onto its rows", {
  y <- subset(make_subset_unit())
  ccc <- y[y$patient_id == "CCC0003", ]
  expect_equal(nrow(ccc), 3L)
  expect_true(all(ccc$gender == "Female"))
  expect_true(all(ccc$catheter_id == "CCC0003_01"))
  expect_identical(ccc$episode_type, c(NA, "repeat", "relapsing"))
  expect_identical(ccc$counts_toward_rate, c(TRUE, TRUE, FALSE))
})

test_that("subset() filters on a patient column", {
  y <- subset(make_subset_unit(), gender == "Female")
  expect_equal(nrow(y), 5L)
  expect_identical(sort(unique(y$patient_id)), c("AAA0001", "CCC0003"))
  # a patient column is repeated on every row of that patient, so all of their catheters and episodes match
  expect_equal(sum(y$patient_id == "CCC0003"), 3L)
})

test_that("subset() filters on a catheter column and returns only the matching rows", {
  y <- subset(make_subset_unit(), procedure_type == "laparoscopic")
  expect_equal(nrow(y), 2L)
  expect_identical(sort(y$catheter_id), c("AAA0001_01", "BBB0002_01"))
  # a catheter that does not match is not in the result
  expect_false("AAA0001_02" %in% y$catheter_id)
})

test_that("subset() filters on an episode column, dropping rows with no episode", {
  y <- subset(make_subset_unit(), episode_type == "repeat")
  expect_equal(nrow(y), 2L)
  expect_identical(sort(y$patient_id), c("AAA0001", "CCC0003"))
  expect_true(all(y$episode_type == "repeat"))
  expect_false("BBB0002" %in% y$patient_id)   # no episode, so NA -> dropped
})

test_that("subset() takes one condition across all three levels", {
  y <- subset(make_subset_unit(),
              gender == "Female" & procedure_type == "open surgical" &
                episode_type == "repeat")
  expect_equal(nrow(y), 2L)
})


test_that("subset() select works like base subset()", {
  x <- make_subset_unit()

  y <- subset(x, select = c(patient_id, gender))
  expect_identical(names(y), c("patient_id", "gender"))
  expect_identical(names(subset(x, select = patient_id:ethnicity)),
                   c("patient_id", "t0", "t1", "new_patient_flag", "n_catheters",
                     "n_episodes", "gender", "ethnicity"))
  expect_false("t0" %in% names(subset(x, select = -c(t0, t1))))
  expect_identical(names(subset(x, select = c("patient_id", "gender"))),
                   c("patient_id", "gender"))
  expect_identical(names(subset(x, select = c(gender, patient_id))),
                   c("gender", "patient_id"))
  both <- subset(x, episode_type == "repeat", select = c(patient_id, catheter_id))
  expect_identical(names(both), c("patient_id", "catheter_id"))
})

test_that("subset() drops rows where the condition is NA", {
  x <- make_subset_unit()
  x$patients$gender[x$patients$patient_id == "BBB0002"] <- NA_character_
  expect_equal(nrow(subset(x, gender == "Female")), 5L)
  expect_equal(nrow(subset(x, gender != "Female")), 0L)
})

test_that("subset() accepts a scalar TRUE or FALSE and an empty result", {
  x <- make_subset_unit()
  expect_equal(nrow(subset(x, TRUE)), 6L)
  none <- subset(x, FALSE)
  expect_identical(names(none), names(subset(x)))   # columns survive
})

test_that("subset() rejects bad conditions and stray arguments", {
  x <- make_subset_unit()
  expect_error(subset(x, n_episodes), "must be logical")
  expect_error(subset(x, no_such_column == 1), "Could not evaluate the `subset`")
  expect_error(subset(x, c(TRUE, FALSE)), "returned 2 value")
  expect_error(subset(x, patients = gender == "Female"), "Unused argument")
})

test_that("subset() copes with a unit whose tibbles are partly or wholly empty", {
  # nothing populated: an empty table, not an error
  e <- subset(new_pd_unit())
  expect_s3_class(e, "tbl_df")
  expect_equal(dim(e), c(0L, 0L))

  # patients and catheters only: no episode columns to join
  y <- subset(make_unit())
  expect_identical(names(y), c("patient_id", "catheter_id"))
  expect_equal(nrow(y), 1L)
})

test_that("subset() reports a join problem", {
  # no catheter_id on the catheters table, so the episodes can't be attached to one
  x <- make_subset_unit()
  x$catheters$catheter_id <- NULL
  expect_error(subset(x), "Cannot join `infections`")

  # no patient_id on the patients table, so the catheters can't be attached to one
  x2 <- make_subset_unit()
  x2$patients$patient_id <- NULL
  expect_error(subset(x2), "Cannot join `catheters`")

  # a child table missing its key column
  x3 <- make_subset_unit()
  x3$infections$catheter_id <- NULL
  expect_error(subset(x3), "`infections` is missing required column")
})

test_that("subset() works on the unit built from the bundled example files", {
  a3 <- system.file("extdata", "a3_2025.xlsx", package = "peridial")
  pe <- system.file("extdata", "pe_2025.xlsx", package = "peridial")
  skip_if_not(nzchar(a3) && nzchar(pe))

  unit <- suppressWarnings(suppressMessages(
    pd_unit(a3, pe, t0 = T0, t1 = T1, unit_id = "Wellington PD Unit")))

  flat <- subset(unit)
  # one row per episode, plus one row for each catheter that has none
  no_episode <- sum(!unit$catheters$catheter_id %in% unit$infections$catheter_id)
  expect_equal(nrow(flat), nrow(unit$infections) + no_episode)
  expect_equal(sum(!is.na(flat$infection_date)), nrow(unit$infections))
  expect_equal(dplyr::n_distinct(flat$patient_id), unit$n_patients)

  female <- subset(unit, gender == "Female")
  expect_equal(dplyr::n_distinct(female$patient_id),
               sum(unit$patients$gender == "Female", na.rm = TRUE))
  expect_true(all(female$gender == "Female"))

  expect_equal(nrow(subset(unit, !is.na(infection_date))), nrow(unit$infections))
})

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
  expect_error(subset(x3), "The infections table is missing required column")
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


# pd_unit(): building a unit from raw files -------------------------------------
# These exercise the whole ingest chain (ingest_read -> ingest_tau ->
# ingest_build -> ingest_tables) through the one public entry point.

bundled_a3 <- function() system.file("extdata", "a3_2025.xlsx", package = "peridial")
bundled_pe <- function() system.file("extdata", "pe_2025.xlsx", package = "peridial")

# Write small A3 / PE files to temp .xlsx. A row is a named list; missing fields are blank.
write_a3 <- function(rows, rename = NULL) {
  skip_if_not_installed("writexl")
  cols <- c("Patient ID", "Date of Birth", "Gender", "Insertion Date", "PD Start Date",
            "PD Stop Date", "Removal Reason", "Dialysis Modality Change",
            "Modality Change Reason", "Date Modality Change", "Date of Death",
            "Cause of Death", "Transplant Date")
  df <- do.call(rbind, lapply(rows, function(r) {
    out <- as.data.frame(stats::setNames(rep(list(NA), length(cols)), cols),
                         check.names = FALSE, stringsAsFactors = FALSE)
    for (nm in names(r)) out[[nm]] <- r[[nm]]
    out
  }))
  for (nm in grep("Date|Insertion", names(df), value = TRUE)) {
    df[[nm]] <- as.Date(df[[nm]])
  }
  df[["Patient ID"]] <- as.character(df[["Patient ID"]])
  if (!is.null(rename)) names(df)[match(names(rename), names(df))] <- unname(rename)
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, path)
  path
}

write_pe <- function(rows) {
  skip_if_not_installed("writexl")
  df <- data.frame(
    `Patient ID` = vapply(rows, function(r) r$id, ""),
    `Date of Infection` = as.Date(vapply(rows, function(r) r$date, "")),
    Organism = vapply(rows, function(r) r$organism %||% "E. coli", ""),
    `Last Dose Antibiotic` = as.Date(vapply(rows, function(r) r$last_dose, "")),
    check.names = FALSE, stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, path)
  path
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# An open catheter for patient `id` that has been running since mid-2024
a3_row <- function(id, ...) {
  base <- list(`Patient ID` = id, `Date of Birth` = "1970-01-01", Gender = "Female",
               `Insertion Date` = "2024-05-01", `PD Start Date` = "2024-06-01")
  utils::modifyList(base, list(...))
}

build_unit <- function(a3, pe, ...) {
  pd_unit(a3, pe, t0 = T0, t1 = T1, ...)
}

# A PE file needs at least one row to be readable; this episode belongs to a patient used as a quiet default
pe_quiet <- function(id = "ABC1234") {
  write_pe(list(list(id = id, date = "2025-03-01", last_dose = "2025-03-15")))
}

test_that("pd_unit() builds a valid unit from the bundled example files", {
  skip_if_not(nzchar(bundled_a3()) && nzchar(bundled_pe()))
  unit <- expect_no_warning(build_unit(bundled_a3(), bundled_pe(), unit_id = "Wellington PD Unit"))

  expect_s3_class(unit, "pd_unit")
  expect_identical(unit$unit_id, "Wellington PD Unit")
  expect_identical(unit$n_patients, 60L)
  expect_identical(unit$n_new, 10L)
  expect_equal(unit$tpyar, 50.94, tolerance = 0.01)
  expect_identical(nrow(unit$patients), 60L)
  expect_identical(nrow(unit$catheters), 60L)
  expect_identical(nrow(unit$infections), 18L)
  expect_identical(sum(unit$infections$counts_toward_rate), 18L)
  expect_no_error(validate_pd_unit(unit))
})

test_that("pd_unit() derives each patient's censoring event from the bundled files", {
  skip_if_not(nzchar(bundled_a3()) && nzchar(bundled_pe()))
  unit <- build_unit(bundled_a3(), bundled_pe())
  counts <- table(unit$patients$transfer_reason, useNA = "no")
  expect_equal(as.list(counts)[c("death", "pd stopped", "permanent transfer to HD", "transplant")],
               list(death = 3L, `pd stopped` = 2L, `permanent transfer to HD` = 2L, transplant = 3L),
               ignore_attr = TRUE)
  censored <- unit$patients[!is.na(unit$patients$transfer_reason), ]
  expect_true(all(!is.na(censored$transfer_date)))
  # no catheter outlives its patient's censoring date
  joined <- merge(unit$catheters, censored[c("patient_id", "transfer_date")], by = "patient_id")
  expect_true(all(!is.na(joined$pd_stop_date) & joined$pd_stop_date <= joined$transfer_date))
})

test_that("pd_unit() attaches every episode to a catheter of the same patient", {
  skip_if_not(nzchar(bundled_a3()) && nzchar(bundled_pe()))
  unit <- build_unit(bundled_a3(), bundled_pe())
  owner <- unit$catheters$patient_id[match(unit$infections$catheter_id, unit$catheters$catheter_id)]
  expect_identical(owner, unit$infections$patient_id)
})

test_that("pd_unit() rejects a bad reporting period", {
  expect_error(pd_unit("a", "b", t0 = "2025-01-01", t1 = T1))
  expect_error(pd_unit("a", "b", t0 = T0, t1 = as.Date(NA)))
  expect_error(pd_unit("a", "b", t0 = c(T0, T0), t1 = T1))
  expect_error(pd_unit("a", "b", t0 = T1, t1 = T0), "t0 must be on or before t1")
})

test_that("pd_unit() reports every problem in the bundled 'modified' A3 file together", {
  path <- system.file("extdata", "a3_2025_modified.xlsx", package = "peridial")
  skip_if_not(nzchar(path) && nzchar(bundled_pe()))
  err <- expect_error(build_unit(path, bundled_pe()), "6 data-quality issue")
  expect_match(conditionMessage(err), "row 40 \\(patient SPD0039\\): missing required value")
  expect_match(conditionMessage(err), "row 41: `40` is not a valid NHI number")
  expect_match(conditionMessage(err), "SPD0053_01: pd_stop_date must be supplied")
  expect_match(conditionMessage(err), "SPD0059_01: pd_stop_date must be supplied")
  expect_match(conditionMessage(err), "SPD0053: no valid PD catheter remains")
  expect_match(conditionMessage(err), "Correct these in the source data and re-run")
})

test_that("pd_unit() errors when the A3 file lacks a required column", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  # drop the PD Start Date column
  df <- readxl::read_excel(a3)
  df[["PD Start Date"]] <- NULL
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, path)
  expect_error(build_unit(path, pe_quiet()), "unit \\(A3\\) file is missing required column\\(s\\): `pd_start_date`")
})

test_that("pd_unit() errors when the PE file lacks a required column", {
  skip_if_not_installed("writexl")
  pe <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(`Patient ID` = "ABC1234", check.names = FALSE), pe)
  expect_error(build_unit(write_a3(list(a3_row("ABC1234"))), pe),
               "infection \\(PE\\) file is missing required column")
})

test_that("pd_unit() maps differently-named columns onto the expected ones", {
  a3 <- write_a3(list(a3_row("ABC1234")),
                 rename = c(`Patient ID` = "NHI", `PD Stop Date` = "Date Stopped PD"))
  unit <- expect_no_warning(build_unit(a3, pe_quiet()))
  expect_identical(unit$patients$patient_id, "ABC1234")
})

test_that("pd_unit() reads death and transplant dates from bare 'Death' / 'Transplanted' headers", {
  a3 <- write_a3(list(a3_row("ABC1234", `Date of Death` = "2025-06-30", `Cause of Death` = "Cardiac"),
                      a3_row("DEF5678", `Transplant Date` = "2025-04-01")),
                 rename = c(`Date of Death` = "Death", `Transplant Date` = "Transplanted"))
  unit <- expect_no_warning(build_unit(a3, pe_quiet()))
  p1 <- unit$patients[unit$patients$patient_id == "ABC1234", ]
  p2 <- unit$patients[unit$patients$patient_id == "DEF5678", ]
  expect_identical(p1$transfer_reason, "death")
  expect_identical(p1$transfer_date, as.Date("2025-06-30"))
  expect_identical(p1$transfer_detail, "Cardiac")
  expect_identical(p2$transfer_reason, "transplant")
  expect_identical(p2$transfer_date, as.Date("2025-04-01"))
})

test_that("pd_unit() closes an open catheter at the date of death", {
  a3 <- write_a3(list(a3_row("ABC1234", `Date of Death` = "2025-06-30", `Cause of Death` = "Cardiac")))
  unit <- build_unit(a3, pe_quiet())
  expect_identical(unit$patients$transfer_reason, "death")
  expect_identical(unit$patients$transfer_date, as.Date("2025-06-30"))
  expect_identical(unit$patients$transfer_detail, "Cardiac")
  expect_identical(unit$catheters$pd_stop_date, as.Date("2025-06-30"))
  expect_equal(unit$tpyar, as.numeric(as.Date("2025-06-30") - T0 + 1) / 365.25)
})

test_that("pd_unit() errors on a catheter that runs past death, naming the fix needed", {
  a3 <- write_a3(list(a3_row("ABC1234", `PD Stop Date` = "2025-09-30", `Removal Reason` = "Death",
                             `Date of Death` = "2025-06-30")))
  expect_error(build_unit(a3, pe_quiet()),
               "catheter\\(s\\) ABC1234_01 have a pd_stop_date after this patient's death on 2025-06-30")
})

test_that("pd_unit() builds once the catheter no longer runs past death", {
  a3 <- write_a3(list(a3_row("ABC1234", `PD Stop Date` = "2025-06-30", `Removal Reason` = "Death",
                             `Date of Death` = "2025-06-30")))
  unit <- expect_no_error(build_unit(a3, pe_quiet()))
  expect_identical(unit$catheters$pd_stop_date, as.Date("2025-06-30"))
})

test_that("pd_unit() treats a catheter removed for transplant as a transplant when no date is given", {
  a3 <- write_a3(list(a3_row("ABC1234", `PD Stop Date` = "2025-04-15", `Removal Reason` = "Transplant")))
  unit <- build_unit(a3, pe_quiet())
  expect_identical(unit$patients$transfer_reason, "transplant")
  expect_identical(unit$patients$transfer_date, as.Date("2025-04-15"))
})

test_that("pd_unit() censors at an 'Any PD to HD' change", {
  a3 <- write_a3(list(a3_row("ABC1234", `Dialysis Modality Change` = "Any PD to HD",
                             `Date Modality Change` = "2025-05-01",
                             `Modality Change Reason` = "Failure")))
  unit <- build_unit(a3, pe_quiet())
  expect_identical(unit$patients$transfer_reason, "permanent transfer to HD")
  expect_identical(unit$patients$transfer_date, as.Date("2025-05-01"))
  expect_identical(unit$patients$transfer_detail, "Failure")
})

test_that("pd_unit() errors when an 'Any PD to HD' change has no reason recorded", {
  a3 <- write_a3(list(a3_row("ABC1234", `Dialysis Modality Change` = "Any PD to HD",
                             `Date Modality Change` = "2025-05-01")))
  expect_error(build_unit(a3, pe_quiet()), "no modality_change_reason")
})

test_that("pd_unit() errors when a transplant or HD transfer is recorded without a date, listing both", {
  a3 <- write_a3(list(
    a3_row("ABC1234", `Dialysis Modality Change` = "Transplant"),
    a3_row("DEF5678", `Dialysis Modality Change` = "Any PD to HD", `Modality Change Reason` = "Failure")))
  err <- expect_error(build_unit(a3, pe_quiet()), "2 data-quality issue")
  expect_match(conditionMessage(err), "Patient ABC1234 has a 'transplant' modality change recorded but no transplant_date")
  expect_match(conditionMessage(err), "Patient DEF5678 has an 'Any PD to HD' modality change recorded but no date_modality_change")
})

test_that("pd_unit(censor_on_last_stop = ) controls whether a closed catheter ends PD", {
  a3 <- write_a3(list(a3_row("ABC1234", `PD Stop Date` = "2025-03-01", `Removal Reason` = "Patient choice")))
  on <- build_unit(a3, pe_quiet())
  expect_identical(on$patients$transfer_reason, "pd stopped")
  off <- build_unit(a3, pe_quiet(), censor_on_last_stop = FALSE)
  expect_true(is.na(off$patients$transfer_reason))
  # the catheter's own stop date still bounds its exposure
  expect_equal(on$tpyar, off$tpyar)
})

test_that("pd_unit() builds more than one catheter per patient, numbered by insertion date", {
  a3 <- write_a3(list(
    a3_row("ABC1234", `Insertion Date` = "2025-02-01", `PD Start Date` = "2025-02-15"),
    a3_row("ABC1234", `Insertion Date` = "2023-01-01", `PD Start Date` = "2023-02-01",
           `PD Stop Date` = "2025-01-15", `Removal Reason` = "Infection")))
  unit <- build_unit(a3, pe_quiet())
  expect_identical(unit$n_patients, 1L)
  expect_setequal(unit$catheters$catheter_id, c("ABC1234_01", "ABC1234_02"))
  expect_identical(unit$catheters$insertion_date[unit$catheters$catheter_id == "ABC1234_01"],
                   as.Date("2023-01-01"))
})

test_that("pd_unit() leaves out patients who were not on PD during the period", {
  a3 <- write_a3(list(
    a3_row("ABC1234"),
    a3_row("DEF5678", `PD Start Date` = "2021-01-01", `Insertion Date` = "2020-12-01",
           `PD Stop Date` = "2022-01-01", `Removal Reason` = "Infection")))
  unit <- expect_no_warning(build_unit(a3, pe_quiet()))
  expect_identical(unit$patients$patient_id, "ABC1234")
})

test_that("pd_unit() flags an incident patient (first PD start inside the period)", {
  a3 <- write_a3(list(a3_row("ABC1234", `Insertion Date` = "2025-02-01", `PD Start Date` = "2025-02-15")))
  unit <- build_unit(a3, pe_quiet())
  expect_identical(unit$n_new, 1L)
  expect_true(unit$patients$new_patient_flag)
})

test_that("pd_unit() errors on an episode with no active catheter", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15"),
                      list(id = "ABC1234", date = "2023-03-01", last_dose = "2023-03-15")))
  expect_error(build_unit(a3, pe), "no active PD catheter on infection_date 2023-03-01")
})

test_that("pd_unit() raises data-quality problems as an error, not a warning", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2023-03-01", last_dose = "2023-03-15")))
  expect_no_warning(try(build_unit(a3, pe), silent = TRUE))
  expect_error(build_unit(a3, pe), "1 data-quality issue\\(s\\) found while building this unit")
})

test_that("pd_unit() no longer has a `strict` argument", {
  expect_false("strict" %in% names(formals(pd_unit)))
  a3 <- write_a3(list(a3_row("ABC1234")))
  expect_error(build_unit(a3, pe_quiet(), strict = TRUE), "unused argument")
})

test_that("pd_unit() can be re-run to success once the source data is corrected", {
  pe_bad <- write_pe(list(list(id = "ABC1234", date = "2023-03-01", last_dose = "2023-03-15")))
  pe_ok <- write_pe(list(list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15")))
  a3 <- write_a3(list(a3_row("ABC1234")))
  expect_error(build_unit(a3, pe_bad), "no active PD catheter")
  unit <- build_unit(a3, pe_ok)
  expect_identical(nrow(unit$infections), 1L)
})

test_that("pd_unit() errors on a row with a blank required value, naming the row", {
  a3 <- write_a3(list(a3_row("ABC1234"), a3_row("DEF5678", `Insertion Date` = NA)))
  expect_error(build_unit(a3, pe_quiet()),
               "row 3 \\(patient DEF5678\\): missing required value\\(s\\) in insertion_date")
})

test_that("pd_unit() errors when a patient is left with no valid catheter", {
  # a removal_reason with no pd_stop_date is rejected by pd_catheter()
  a3 <- write_a3(list(a3_row("ABC1234", `Removal Reason` = "Infection")))
  err <- expect_error(build_unit(a3, pe_quiet()), "no valid PD catheter remains")
  expect_match(conditionMessage(err), "Catheter ABC1234_01: pd_stop_date must be supplied")
})

test_that("pd_unit() chains episodes so a repeat episode is classified", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15", organism = "E. coli"),
                      list(id = "ABC1234", date = "2025-03-20", last_dose = "2025-04-03", organism = "E. coli")))
  unit <- build_unit(a3, pe)
  expect_identical(unit$infections$episode_type, c(NA, "relapsing"))
  expect_identical(unit$infections$counts_toward_rate, c(TRUE, FALSE))
  expect_identical(unit$catheters$n_peritonitis_episodes, 1)
})

test_that("pd_unit() returns an empty, valid unit when nobody was on PD in the period", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  unit <- pd_unit(a3, pe_quiet(), t0 = as.Date("2015-01-01"), t1 = as.Date("2015-12-31"))
  expect_identical(unit$n_patients, 0L)
  expect_identical(unit$tpyar, 0)
  expect_identical(nrow(unit$patients), 0L)
})

test_that("pd_unit() errors on a patient_id that is not a valid NHI, naming the sheet row", {
  a3 <- write_a3(list(a3_row("ABC1234"), a3_row("40")))
  err <- expect_error(build_unit(a3, pe_quiet()), "1 data-quality issue")
  expect_match(conditionMessage(err), "unit \\(A3\\) file, row 3: `40` is not a valid NHI number")
})

test_that("pd_unit() accepts lower-case NHIs and stores them upper case", {
  a3 <- write_a3(list(a3_row("abc1234"), a3_row("Def5678")))
  unit <- expect_no_error(build_unit(a3, pe_quiet()))
  expect_setequal(unit$patients$patient_id, c("ABC1234", "DEF5678"))
})

test_that("pd_unit() matches PE episodes to A3 patients regardless of case", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "abc1234", date = "2025-03-01", last_dose = "2025-03-15")))
  unit <- expect_no_error(build_unit(a3, pe))
  expect_identical(nrow(unit$infections), 1L)
})

test_that("pd_unit() rejects an NHI that is not three letters then four digits", {
  for (bad in c("ABC123", "ABC12345", "ABC1D23", "ABC12DV", "AB12345", "1234567")) {
    a3 <- write_a3(list(a3_row("DEF5678"), a3_row(bad)))
    expect_error(build_unit(a3, pe_quiet("DEF5678")),
                 paste0("`", bad, "` is not a valid NHI number"), fixed = TRUE)
  }
})

test_that("pd_unit() reports the original sheet row even after earlier rows were dropped", {
  a3 <- write_a3(list(a3_row("ABC1234", `Insertion Date` = NA),   # row 2: blank required value
                      a3_row("DEF5678"),                        # row 3: fine
                      a3_row("12")))                            # row 4: not an NHI
  err <- expect_error(build_unit(a3, pe_quiet("DEF5678")), "2 data-quality issue")
  expect_match(conditionMessage(err), "row 2 \\(patient ABC1234\\): missing required value")
  expect_match(conditionMessage(err), "row 4: `12` is not a valid NHI")
})

test_that("pd_unit() flags an invalid NHI in the PE file", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15"),
                      list(id = "ABC1", date = "2025-04-01", last_dose = "2025-04-15")))
  err <- expect_error(build_unit(a3, pe), "1 data-quality issue")
  expect_match(conditionMessage(err), "infection \\(PE\\) file, row 3: `ABC1` is not a valid NHI number")
})

test_that("pd_unit() errors when a PE patient has no row in the A3 file, rather than dropping the episode", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15"),
                      list(id = "DEF5678", date = "2025-04-01", last_dose = "2025-04-15")))
  err <- expect_error(build_unit(a3, pe), "1 data-quality issue")
  expect_match(conditionMessage(err), "infection \\(PE\\) file, row 3: patient DEF5678 has no row in the unit \\(A3\\) file")
})

test_that("pd_unit() catches a mistyped PE patient_id in the bundled files (no silent episode loss)", {
  skip_if_not(nzchar(bundled_a3()) && nzchar(bundled_pe()))
  skip_if_not_installed("writexl")
  pe <- readxl::read_excel(bundled_pe())
  pe[["Patient ID"]][1] <- sub("SPD00", "SPD0", pe[["Patient ID"]][1])   # SPD0003 -> SPD003
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(pe, path)
  expect_error(build_unit(bundled_a3(), path), "row 2: `SPD003` is not a valid NHI number")
})

test_that("pd_unit() standardises organism spellings", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(
    list(id = "ABC1234", date = "2025-02-01", last_dose = "2025-02-15", organism = "  staphylococcus AUREUS "),
    list(id = "ABC1234", date = "2025-06-01", last_dose = "2025-06-15", organism = "Escherichia coli, Klebsiella pneumoniae")))
  unit <- build_unit(a3, pe)
  expect_identical(unit$infections$organisms, c("S. aureus", "E. coli, Klebsiella"))
})

test_that("pd_unit() treats 'Culture negative' as culture negative", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-02-01", last_dose = "2025-02-15",
                           organism = "Culture negative")))
  unit <- build_unit(a3, pe)
  expect_identical(unit$infections$organisms, "negative")
  expect_identical(summarise_infections(unit)$culture_negative_n, 1L)
})

test_that("pd_unit() classifies two spellings of the same organism as a relapse", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(
    list(id = "ABC1234", date = "2025-03-01", last_dose = "2025-03-15", organism = "Escherichia coli"),
    list(id = "ABC1234", date = "2025-03-20", last_dose = "2025-04-03", organism = "e. coli")))
  unit <- build_unit(a3, pe)
  expect_identical(unit$infections$episode_type, c(NA_character_, "relapsing"))
})

test_that("pd_unit() errors on an organism that is not on the form's list, naming the row", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(
    list(id = "ABC1234", date = "2025-02-01", last_dose = "2025-02-15", organism = "E. coli"),
    list(id = "ABC1234", date = "2025-06-01", last_dose = "2025-06-15", organism = "Staph auerus")))
  err <- expect_error(build_unit(a3, pe), "1 data-quality issue")
  expect_match(conditionMessage(err), "infection \\(PE\\) file, row 3: organism `Staph auerus`")
})

test_that("pd_unit() errors on a real but rarer organism that is not in the accepted list", {
  a3 <- write_a3(list(a3_row("ABC1234")))
  pe <- write_pe(list(list(id = "ABC1234", date = "2025-02-01", last_dose = "2025-02-15",
                           organism = "Roseomonas gilardii")))
  expect_error(build_unit(a3, pe), "organism `Roseomonas gilardii` is not in the accepted organism list")
})

test_that("pd_unit() on the bundled data reports its three culture-negative episodes", {
  skip_if_not(nzchar(bundled_a3()) && nzchar(bundled_pe()))
  unit <- build_unit(bundled_a3(), bundled_pe())
  expect_identical(summarise_infections(unit)$culture_negative_n, 3L)
  expect_true(all(names(summarise_infections(unit)$organisms) %in% organism_names))
})

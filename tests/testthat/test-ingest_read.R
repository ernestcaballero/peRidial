# Tests for R/ingest_read.R: reading, naming and cleaning the raw A3 / PE files


# standardise_names()


test_that("standardise_names() splits camelCase and strips punctuation and edge underscores", {
  df <- data.frame(`PatientId` = 1, ` Date-of / Death? ` = 1, `_odd__name_` = 1,
                   check.names = FALSE)
  expect_named(standardise_names(df), c("patient_id", "date_of_death", "odd_name"))
})

test_that("standardise_names() does not split acronym runs (documented limitation)", {
  df <- data.frame(InterimHDCode = 1)
  expect_named(standardise_names(df), "interim_hdcode")
})

test_that("standardise_names() leaves the data untouched", {
  df <- data.frame(`A B` = 1:3, check.names = FALSE)
  expect_equal(standardise_names(df)$a_b, 1:3)
})


# matches_keywords()

test_that("matches_keywords() accepts an exact name", {
  rule <- list(exact = c("nhi", "id"))
  expect_true(matches_keywords("nhi", rule))
  expect_false(matches_keywords("nhi_number", rule))
})


test_that("matches_keywords() requires all of `all`", {
  rule <- list(all = c("pd", "start", "date"))
  expect_true(matches_keywords("pd_start_date", rule))
  expect_false(matches_keywords("pd_stop_date", rule))
})


test_that("matches_keywords() combines `all` and `any`", {
  rule <- list(all = "patient", any = c("id", "nhi"))
  expect_true(matches_keywords("patient_nhi", rule))
  expect_false(matches_keywords("patient_name", rule))
  expect_false(matches_keywords("nhi", rule))
})


# map_columns()

test_that("map_columns() renames a differently-named column to the expected name", {
  df <- standardise_names(data.frame(NHI = "A1", `Date of Birth` = 1,
                                     check.names = FALSE))
  out <- map_columns(df, a3_spec, "unit (A3)")
  expect_true(all(c("patient_id", "date_of_birth") %in% names(out)))
  expect_false("nhi" %in% names(out))
})

test_that("map_columns() leaves a column that already has the expected name alone", {
  df <- data.frame(patient_id = "A1", other = 1)
  out <- map_columns(df, a3_spec, "unit (A3)")
  expect_identical(out, df)
})


test_that("map_columns() logs, and uses the first, when several columns could be the key", {
  log <- new_issue_log()
  df <- data.frame(patient_nhi = "A1", patient_identifier = "B2")
  out <- map_columns(df, a3_spec, "unit (A3)", log)
  expect_identical(names(out), c("patient_id", "patient_identifier"))
  expect_length(log$get(), 1)
  expect_match(log$get(), "unit \\(A3\\) file has 2 columns that could be `patient_id`")
})

test_that("map_columns() resolves the modality-change columns in specificity order", {
  df <- data.frame(modality_change_reason = 1, date_modality_change = 2,
                   dialysis_modality_change = 3)
  out <- map_columns(df, a3_spec, "unit (A3)")
  expect_identical(names(out), names(df))

  renamed <- data.frame(reason_for_modality_change = 1, modality_change_date = 2,
                        modality_change_type = 3)
  out2 <- map_columns(renamed, a3_spec, "unit (A3)")
  expect_identical(names(out2), c("modality_change_reason", "date_modality_change",
                                  "dialysis_modality_change"))
})


test_that("map_columns() tells the cause of death from the date of death, in either column order", {
  a <- standardise_names(data.frame(`Date of Death` = 1, `Cause of Death` = 2, check.names = FALSE))
  b <- standardise_names(data.frame(`Cause of Death` = 1, Death = 2, check.names = FALSE))
  c3 <- standardise_names(data.frame(`Reason for Death` = 1, `Death Date` = 2, check.names = FALSE))
  expect_named(map_columns(a, a3_spec, "unit (A3)"), c("date_of_death", "cause_of_death"))
  expect_named(map_columns(b, a3_spec, "unit (A3)"), c("cause_of_death", "date_of_death"))
  expect_named(map_columns(c3, a3_spec, "unit (A3)"), c("cause_of_death", "date_of_death"))
})


test_that("map_columns() maps PE headers, with catheter_removed_date taking priority", {
  df <- standardise_names(data.frame(
    `Patient ID` = "A1", `Date of Infection` = 1, Organism = "x",
    `Last Dose Antibiotic` = 1, `Catheter Removed` = TRUE,
    `Catheter Removed Date` = 1, `Days Hospitalised` = 2,
    `Overnight Hospitalisation` = "Yes", check.names = FALSE))
  out <- map_columns(df, pe_spec, "infection (PE)")
  expect_identical(names(out), names(df))
})


# require_cols()

test_that("require_cols() passes quietly when every column is present", {
  df <- data.frame(a = 1, b = 2)
  expect_invisible(require_cols(df, c("a", "b"), "unit (A3)"))
  expect_true(require_cols(df, "a", "unit (A3)"))
})

test_that("require_cols() errors, naming the file, the missing columns and what was found", {
  df <- data.frame(a = 1)
  expect_error(require_cols(df, c("a", "b", "c"), "unit (A3)"),
               "unit \\(A3\\) file is missing required column\\(s\\): `b`, `c`")
  expect_error(require_cols(df, "b", "unit (A3)"), "Columns found after name standardisation: a")
})


test_that("require_cols() records the problem in the log before it errors", {
  log <- new_issue_log()
  expect_error(require_cols(data.frame(a = 1), "b", "PE", log))
  expect_length(log$get(), 1)
  expect_match(log$get(), "missing required column")
})


# ensure_cols() ----------------------------------------------------------------

test_that("ensure_cols() adds missing columns as all-NA and keeps existing ones", {
  df <- data.frame(a = 1:2)
  out <- ensure_cols(df, c("a", "b", "c"))
  expect_named(out, c("a", "b", "c"))
  expect_identical(out$a, 1:2)
  expect_true(all(is.na(out$b)))
  expect_length(out$c, 2)
})


test_that("ensure_cols() works on a zero-row frame", {
  out <- ensure_cols(data.frame(a = integer(0)), "b")
  expect_identical(nrow(out), 0L)
  expect_true("b" %in% names(out))
})


# as_date_safe()

test_that("as_date_safe() returns a Date unchanged", {
  d <- as.Date(c("2025-01-01", NA))
  expect_identical(as_date_safe(d), d)
})

test_that("as_date_safe() converts readxl POSIXct dates without shifting the day", {
  x <- as.POSIXct("2025-03-04 00:00:00", tz = "UTC")
  expect_identical(as_date_safe(x), as.Date("2025-03-04"))
  old_tz <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz), add = TRUE)
  Sys.setenv(TZ = "Pacific/Auckland")
  expect_identical(as_date_safe(x), as.Date("2025-03-04"))
})

test_that("as_date_safe() converts an Excel serial number", {
  expect_identical(as_date_safe(45000), as.Date("2023-03-15"))
})

test_that("as_date_safe() parses ISO date strings and tolerates padding and NA", {
  out <- as_date_safe(c("2025-01-31", " 2025-02-01 ", NA))
  expect_identical(out, as.Date(c("2025-01-31", "2025-02-01", NA)))
})

test_that("as_date_safe() warns, naming the column, about values it cannot parse", {
  expect_warning(out <- as_date_safe(c("2025-01-01", "not a date"), "my_col"),
                 "Could not parse 1 value\\(s\\) in `my_col`")
  expect_identical(out, as.Date(c("2025-01-01", NA)))
})


test_that("as_date_safe() does not warn for an all-NA logical column", {
  expect_no_warning(out <- as_date_safe(c(NA, NA)))
  expect_true(all(is.na(out)))
})


# as_logical_safe()

test_that("as_logical_safe() passes logicals through", {
  expect_identical(as_logical_safe(c(TRUE, NA, FALSE)), c(TRUE, NA, FALSE))
})

test_that("as_logical_safe() treats non-zero numbers as TRUE", {
  expect_identical(as_logical_safe(c(1, 0, 2)), c(TRUE, FALSE, TRUE))
})

test_that("as_logical_safe() reads yes/no style text, ignoring case and padding", {
  x <- c("Yes", " y ", "TRUE", "1", "No", "n", "false", "0", "")
  expect_identical(as_logical_safe(x),
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("as_logical_safe() gives NA for unrecognised text and NA input", {
  expect_identical(as_logical_safe(c("maybe", NA)), c(NA, NA))
})


# new_issue_log() / report_issues()

test_that("new_issue_log() starts empty and collects pasted messages in order", {
  log <- new_issue_log()
  expect_identical(log$get(), character(0))
  log$add("first ", 1)
  log$add("second")
  expect_identical(log$get(), c("first 1", "second"))
})


test_that("report_issues() raises one error listing every issue", {
  log <- new_issue_log()
  log$add("problem A")
  log$add("problem B")
  expect_error(report_issues(log), "2 data-quality issue\\(s\\) found")
  expect_error(report_issues(log), "problem A")
  expect_error(report_issues(log), "problem B")
  expect_error(report_issues(log), "Correct these in the source data and re-run")
})


# is_blank_cell()

test_that("is_blank_cell() flags NA, empty and whitespace-only text", {
  expect_identical(is_blank_cell(c("a", "", "  ", NA)), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("is_blank_cell() flags only NA for non-character columns", {
  expect_identical(is_blank_cell(c(1, NA)), c(FALSE, TRUE))
  expect_identical(is_blank_cell(as.Date(c("2025-01-01", NA))), c(FALSE, TRUE))
})


# check_required_cells()

test_that("check_required_cells() returns the data unchanged when nothing is blank", {
  df <- data.frame(patient_id = c("A", "B"), dob = as.Date(c("1980-01-01", "1990-01-01")))
  log <- new_issue_log()
  expect_identical(check_required_cells(df, c("patient_id", "dob"), "unit (A3)", log), df)
  expect_length(log$get(), 0)
})

test_that("check_required_cells() drops rows with a blank required value and logs each", {
  df <- data.frame(patient_id = c("A", "B", "C"),
                   dob = as.Date(c("1980-01-01", NA, "1990-01-01")),
                   stringsAsFactors = FALSE)
  log <- new_issue_log()
  out <- check_required_cells(df, c("patient_id", "dob"), "unit (A3)", log)
  expect_identical(out$patient_id, c("A", "C"))
  expect_length(log$get(), 1)
  # row 2 of the data, +1 for the header row of the sheet
  expect_match(log$get(), "row 3 \\(patient B\\): missing required value\\(s\\) in dob")
})


test_that("check_required_cells() copes with a row that has no patient_id", {
  df <- data.frame(patient_id = c(NA, "B"), dob = c(1, 2))
  log <- new_issue_log()
  out <- check_required_cells(df, c("patient_id", "dob"), "unit (A3)", log)
  expect_identical(out$patient_id, "B")
  expect_match(log$get(), "no patient_id")
})

test_that("check_required_cells() handles a zero-row frame and a custom header offset", {
  empty <- data.frame(patient_id = character(0))
  expect_identical(check_required_cells(empty, "patient_id", "PE", new_issue_log()), empty)

  log <- new_issue_log()
  check_required_cells(data.frame(patient_id = "A", x = NA), "x", "PE", log, header_rows = 3L)
  expect_match(log$get(), "row 4")
})

test_that("check_required_cells() errors if a required column is not in the data", {
  expect_error(check_required_cells(data.frame(a = 1), "b", "PE", new_issue_log()))
})


test_that("check_required_cells() reports the stamped sheet row, not the position", {
  df <- data.frame(patient_id = c("A", "B"), x = c(1, NA), .sheet_row = c(7L, 12L))
  log <- new_issue_log()
  check_required_cells(df, "x", "PE", log)
  expect_match(log$get(), "row 12 ")
})


# is_valid_nhi() ----------------------------------------------------------------

test_that("is_valid_nhi() accepts three letters followed by four digits", {
  expect_true(all(is_valid_nhi(c("ABC1234", "SPD0001", "abc1234", "aBC1234"))))
})


test_that("is_valid_nhi() rejects anything else", {
  bad <- c("40", "SPD001", "SPD00001", "ABC123", "ABC12345", "ABC12DV", " ABC1234",
           "ABC 1234", "AB12345", "A1C1234", "1234567", "ABCDEFG", "ABC-123", "", NA)
  expect_false(any(is_valid_nhi(bad)))
})

test_that("is_valid_nhi() is vectorised and coerces non-character input", {
  expect_identical(is_valid_nhi(c("ABC1234", "x", NA)), c(TRUE, FALSE, FALSE))
  expect_identical(is_valid_nhi(1234567), FALSE)
  expect_identical(is_valid_nhi(character(0)), logical(0))
})


# check_patient_ids() -----------------------------------------------------------

test_that("check_patient_ids() keeps valid rows and logs nothing", {
  df <- data.frame(patient_id = c("ABC1234", "DEF5678"))
  log <- new_issue_log()
  expect_identical(check_patient_ids(df, "unit (A3)", log), df)
  expect_length(log$get(), 0)
})

test_that("check_patient_ids() drops and logs each invalid id with its row", {
  df <- data.frame(patient_id = c("ABC1234", "40", "DEF5678", "xyz"), v = 1:4)
  log <- new_issue_log()
  out <- check_patient_ids(df, "unit (A3)", log)
  expect_identical(out$patient_id, c("ABC1234", "DEF5678"))
  expect_length(log$get(), 2)
  expect_match(log$get()[1], "unit \\(A3\\) file, row 3: `40` is not a valid NHI number")
  expect_match(log$get()[2], "row 5: `xyz` is not a valid NHI number")
})

test_that("check_patient_ids() trims whitespace and stores the upper-case id", {
  df <- data.frame(patient_id = c(" abc1234 ", "ABC1234\t", "Def5678"))
  out <- check_patient_ids(df, "unit (A3)", new_issue_log())
  expect_identical(out$patient_id, c("ABC1234", "ABC1234", "DEF5678"))
})

test_that("check_patient_ids() leaves blank ids to check_required_cells()", {
  df <- data.frame(patient_id = c(NA, "", "  ", "ABC1234"))
  log <- new_issue_log()
  expect_identical(nrow(check_patient_ids(df, "unit (A3)", log)), 4L)
  expect_length(log$get(), 0)
})


test_that("check_patient_ids() handles an empty frame and a missing column", {
  empty <- data.frame(patient_id = character(0))
  expect_identical(check_patient_ids(empty, "PE", new_issue_log()), empty)
  expect_error(check_patient_ids(data.frame(a = 1), "PE", new_issue_log()))
})


# check_known_patients() --------------------------------------------------------

test_that("check_known_patients() keeps rows whose patient is in the A3 file", {
  df <- data.frame(patient_id = c("ABC1234", "DEF5678"))
  log <- new_issue_log()
  expect_identical(check_known_patients(df, c("ABC1234", "DEF5678", "GHJ9012"), "PE", log), df)
  expect_length(log$get(), 0)
})

test_that("check_known_patients() drops and logs an unknown patient with its row", {
  df <- data.frame(patient_id = c("ABC1234", "ZZZ9999"), v = 1:2)
  log <- new_issue_log()
  out <- check_known_patients(df, "ABC1234", "infection (PE)", log)
  expect_identical(out$patient_id, "ABC1234")
  expect_match(log$get(), "infection \\(PE\\) file, row 3: patient ZZZ9999 has no row in the unit \\(A3\\) file")
})


test_that("check_known_patients() trims ids and ignores blanks", {
  df <- data.frame(patient_id = c(" abc1234", NA, ""))
  log <- new_issue_log()
  expect_identical(nrow(check_known_patients(df, "ABC1234", "PE", log)), 3L)
  expect_length(log$get(), 0)
})

test_that("check_known_patients() handles an empty frame", {
  empty <- data.frame(patient_id = character(0))
  expect_identical(check_known_patients(empty, "ABC1234", "PE", new_issue_log()), empty)
})

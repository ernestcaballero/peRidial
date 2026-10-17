# Tests for R/ingest_read.R: reading, naming and cleaning the raw A3 / PE files


# standardise_names()

test_that("standardise_names() produces formatted columns names (snakecase)", {
  df <- data.frame(`PatientId` = 1, ` Date-of / Death? ` = 1, `_odd__name_` = 1,
                   check.names = FALSE)
  expect_named(standardise_names(df), c("patient_id", "date_of_death", "odd_name"))
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

test_that("map_columns() maps differently-named PE headers", {
  df <- standardise_names(data.frame(
    `Patient NHI` = "A1", `Infection Date` = 1, `Organism Cultured` = "x",
    `Final Antibiotic Dose Date` = 1, `Catheter Removal Date` = 1,
    check.names = FALSE))
  out <- map_columns(df, pe_spec, "infection (PE)")
  expect_identical(names(out), c("patient_id", "date_of_infection", "organism",
                                 "last_dose_antibiotic", "catheter_removed_date"))
})

# require_cols()

test_that("require_cols() errors, naming the file, the missing columns and what was found", {
  df <- data.frame(a = 1)
  expect_error(require_cols(df, c("a", "b", "c"), "unit (A3)"),
               "unit \\(A3\\) file is missing required column\\(s\\): `b`, `c`")
  expect_error(require_cols(df, "b", "unit (A3)"), "Columns found after name standardisation: a")
})

test_that("require_cols() records the problem in the log and errors", {
  log <- new_issue_log()
  expect_error(require_cols(data.frame(a = 1), "b", "PE", log))
  expect_match(log$get(), "missing required column")
})

# ensure_cols()

test_that("ensure_cols() adds missing columns as all-NA and keeps existing ones", {
  df <- data.frame(a = 1:2)
  out <- ensure_cols(df, c("a", "b", "c"))
  expect_named(out, c("a", "b", "c"))
  expect_identical(out$a, 1:2)
  expect_true(all(is.na(out$b)))
  expect_length(out$c, 2)
})

test_that("ensure_cols() works on a dataframe with no rows", {
  out <- ensure_cols(data.frame(a = integer(0)), "b")
  expect_identical(nrow(out), 0L)
  expect_named(out, c("a", "b"))
})

# as_date_safe()

test_that("as_date_safe() returns a Date unchanged", {
  d <- as.Date(c("2025-01-01", NA))
  expect_identical(as_date_safe(d), d)
})

test_that("as_date_safe() converts an Excel serial number", {
  expect_identical(as_date_safe(45000), as.Date("2023-03-15"))
})

test_that("as_date_safe() reads day-first text dates, as a CSV or data frame", {
  out <- as_date_safe(c("31/01/2025", "1/2/2025", "03-04-2025", "05.06.2025", "2025/07/08"))
  expect_identical(out, as.Date(c("2025-01-31", "2025-02-01", "2025-04-03",
                                  "2025-06-05", "2025-07-08")))
})

test_that("as_date_safe() copes with a column that mixes formats and blanks", {
  out <- as_date_safe(c("2025-01-31", " 2025-02-01 ", "01/02/2025", "", NA, "2025-03-04 00:00:00"))
  expect_identical(out, as.Date(c("2025-01-31", "2025-02-01", "2025-02-01", NA, NA, "2025-03-04")))
})

test_that("as_date_safe() will not read a two-digit year as year 00YY", {
  expect_warning(out <- as_date_safe("31/01/25", "my_col"),
                 "Could not parse 1 value\\(s\\) in `my_col`")
  expect_identical(out, as.Date(NA))
})

test_that("as_date_safe() takes the calendar day in the value's own time zone", {
  x <- as.POSIXct("2025-03-04 00:00:00", tz = "Pacific/Auckland")   # 2025-03-03 11:00 UTC
  expect_identical(as_date_safe(x), as.Date("2025-03-04"))
  expect_identical(as_date_safe(as.POSIXct("2025-03-04", tz = "UTC")), as.Date("2025-03-04"))
})

# as_logical_safe()

test_that("as_logical_safe() reads yes/no text, ignoring case and padding, and gives NA for the unrecognised", {
  expect_identical(as_logical_safe(c(TRUE, NA, FALSE)), c(TRUE, NA, FALSE))     # logicals pass through
  x <- c("Yes", " y ", "TRUE", "1", "No", "n", "false", "0", "")
  expect_identical(as_logical_safe(x),
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
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

test_that("new_issue_log() keeps errors and warnings apart", {
  log <- new_issue_log()
  log$add("bad id")
  log$add("assumed", level = "warning")
  expect_identical(log$get(), c("bad id", "assumed"))
  expect_identical(log$get("error"), "bad id")
  expect_identical(log$get("warning"), "assumed")
})

# is_blank_cell()

test_that("is_blank_cell() flags NA, empty and whitespace-only text, and only NA for other types", {
  expect_identical(is_blank_cell(c("a", "", "  ", NA)), c(FALSE, TRUE, TRUE, TRUE))
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

test_that("check_required_cells() reports the stamped sheet row, not the position", {
  df <- data.frame(patient_id = c("A", "B"), x = c(1, NA), .sheet_row = c(7L, 12L))
  log <- new_issue_log()
  check_required_cells(df, "x", "PE", log)
  expect_match(log$get(), "row 12 ")
})

# is_valid_nhi()

test_that("is_valid_nhi() accepts three letters followed by four digits", {
  expect_true(all(is_valid_nhi(c("ABC1234", "SPD0001", "abc1234", "aBC1234"))))
  bad <- c("40", "SPD001", "SPD00001", "ABC123", "ABC12345", "ABC12DV", " ABC1234",
           "ABC 1234", "AB12345", "A1C1234", "1234567", "ABCDEFG", "ABC-123", "", NA)
  expect_false(any(is_valid_nhi(bad)))
})

# check_patient_ids()

test_that("check_patient_ids() drops and logs each invalid id with its row", {
  df <- data.frame(patient_id = c("ABC1234", "40", "DEF5678", "xyz"), v = 1:4)
  log <- new_issue_log()
  out <- check_patient_ids(df, "unit (A3)", log)
  expect_identical(out$patient_id, c("ABC1234", "DEF5678"))
  expect_identical(
    startsWith(log$get(), c("The unit (A3) file, row 3: `40` is not a valid NHI number",
                            "The unit (A3) file, row 5: `xyz` is not a valid NHI number")),
    c(TRUE, TRUE))
})

test_that("check_patient_ids() trims whitespace and stores the upper-case id", {
  df <- data.frame(patient_id = c(" abc1234 ", "ABC1234\t", "Def5678"))
  out <- check_patient_ids(df, "unit (A3)", new_issue_log())
  expect_identical(out$patient_id, c("ABC1234", "ABC1234", "DEF5678"))
})

# check_known_patients()

test_that("check_known_patients() drops and logs an unknown patient with its row", {
  df <- data.frame(patient_id = c("ABC1234", "ZZZ9999"), v = 1:2)
  log <- new_issue_log()
  out <- check_known_patients(df, "ABC1234", "infection (PE)", log)
  expect_identical(out$patient_id, "ABC1234")
  expect_match(log$get(), "infection \\(PE\\) file, row 3: patient ZZZ9999 has no row in the unit \\(A3\\) file")
})

# read_input()

test_that("read_input() names the argument when the file is missing or the input is the wrong type", {
  expect_error(read_input(file.path(tempdir(), "no_such_file.csv"), "unit_data"),
               "`unit_data`: file not found")
  expect_error(read_input("", "unit_data"), "must be a single file path")
  expect_error(read_input(c("a.csv", "b.csv"), "unit_data"), "must be a single file path")
  expect_error(read_input(42, "infection_data"), "`infection_data` must be")
  expect_error(read_input(list(a = 1), "infection_data"), "`infection_data` must be")
})

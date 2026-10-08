# organism_spellings / match_organism() ----------------------------------------

test_that("the organism table has unique, comma-free standard names", {
  expect_equal(length(organism_names), 34)
  expect_false(anyDuplicated(organism_names) > 0)
  expect_false(any(grepl(",", organism_names, fixed = TRUE)))
  expect_true(all(c("negative", "S. aureus", "E. coli", "No culture taken") %in% organism_names))
})

test_that("no spelling is claimed by two different organisms", {
  keys <- organism_key(organism_spellings$spelling)
  n_names <- tapply(organism_spellings$name, keys, function(v) length(unique(v)))
  expect_true(all(n_names == 1))
  # and no organism's standard name is claimed by another organism
  own <- organism_key(organism_names)
  expect_identical(unname(organism_lookup[own]), organism_names)
})

test_that("every spelling and standard name maps to its own standard name", {
  expect_identical(match_organism(organism_spellings$spelling), organism_spellings$name)
  expect_identical(match_organism(organism_names), organism_names)
})

test_that("the rarer form organisms are deliberately not accepted", {
  expect_true(all(is.na(match_organism(c("Stenotrophomonas maltophilia", "Burkholderia cepacia",
                                         "Pseudomonas stutzeri", "Citrobacter koseri",
                                         "Coliforms", "Roseomonas gilardii", "Not reported")))))
})

test_that("organism_key() ignores case, punctuation, whitespace and 'sp'", {
  expect_identical(organism_key(c("E. coli", "e coli", "  E.COLI ", "E-coli")), rep("e coli", 4))
  expect_identical(organism_key("Klebsiella Sp"), "klebsiella")
  expect_identical(organism_key("Serratia species"), "serratia")
})

test_that("match_organism() standardises the spellings seen in practice", {
  expect_identical(match_organism(c("Staphylococcus aureus", "S. aureus", "staph aureus",
                                    "  S aureus ", "STAPHYLOCOCCUS  AUREUS")),
                   rep("S. aureus", 5))
  expect_identical(match_organism(c("Escherichia coli", "e. coli", "E coli", "ECOLI")),
                   c("E. coli", "E. coli", "E. coli", NA))
  expect_identical(match_organism("Staphylococcus epidermidis"), "S. epidermidis")
  expect_identical(match_organism("Klebsiella pneumoniae"), "Klebsiella")
  expect_identical(match_organism("Pseudomonas aeruginosa"), "P. aeruginosa")
  expect_identical(match_organism("Enterococcus faecalis"), "Enterococcus")
})

test_that("MRSA and non-MRSA S. aureus stay distinct", {
  expect_identical(match_organism(c("MRSA", "MSSA", "Staphylococcus aureus")),
                   c("S. aureus (MRSA)", "S. aureus (non-MRSA)", "S. aureus"))
})

test_that("culture negative maps to the package's 'negative' marker", {
  expect_identical(match_organism(c("Culture negative", "culture-negative", "Negative")),
                   rep("negative", 3))
})

test_that("no culture taken is not culture negative", {
  expect_identical(match_organism("No culture taken"), "No culture taken")
})

test_that("numeric form codes are not accepted (the raw files carry names only)", {
  expect_true(all(is.na(match_organism(c("1", "23", "40")))))
})

test_that("match_organism() returns NA for unrecognised or missing values", {
  expect_identical(match_organism(c("Staph auerus", "banana", "", NA)), rep(NA_character_, 4))
})


# standardise_organism_cell() ---------------------------------------------------

test_that("a cell naming one organism gives one standard name", {
  res <- standardise_organism_cell("Staphylococcus aureus")
  expect_identical(res$std, "S. aureus")
  expect_length(res$unknown, 0)
})

test_that("a cell naming several organisms is split on , ; / & + and 'and'", {
  for (cell in c("S. aureus, E. coli", "S. aureus; E. coli", "S. aureus / E. coli",
                 "S. aureus & E. coli", "S. aureus + E. coli", "S. aureus and E. coli",
                 "S. aureus,E. coli")) {
    expect_identical(standardise_organism_cell(cell)$std, c("S. aureus", "E. coli"), info = cell)
  }
})

test_that("the form's own labels that contain commas are not split", {
  expect_identical(standardise_organism_cell("Coag - Neg Staph, Staph Epidermidis")$std, "S. epidermidis")
  expect_identical(
    standardise_organism_cell("Streptococcus Viridians Group (Sangius, Bovis, Etc), E. coli")$std,
    c("Strep viridans group", "E. coli"))
})

test_that("Fungi/Yeast is kept whole rather than split on '/'", {
  expect_identical(standardise_organism_cell("Fungi/Yeast, Other (Specify)")$std, "Fungi/Yeast (other)")
})

test_that("duplicates within a cell collapse", {
  expect_identical(standardise_organism_cell("E. coli, Escherichia coli")$std, "E. coli")
  expect_identical(standardise_organism_cell("Enterococcus faecalis, Enterococcus faecium")$std,
                   "Enterococcus")
})

test_that("unrecognised pieces are returned, recognised ones kept", {
  res <- standardise_organism_cell("E. coli, Staph auerus, banana")
  expect_identical(res$std, "E. coli")
  expect_identical(res$unknown, c("Staph auerus", "banana"))
})


# check_organisms() -------------------------------------------------------------

test_that("check_organisms() replaces each cell with the standard names and logs nothing", {
  df <- data.frame(organism = c("Staphylococcus aureus", "e. coli, Klebsiella pneumoniae", "Culture negative"))
  log <- new_issue_log()
  out <- check_organisms(df, "infection (PE)", log)
  expect_identical(out$organism, c("S. aureus", "E. coli, Klebsiella", "negative"))
  expect_length(log$get(), 0)
})

test_that("check_organisms() drops and logs a row with an unrecognised organism, with its sheet row", {
  df <- data.frame(organism = c("E. coli", "Staph auerus", "Klebsiella"), v = 1:3)
  log <- new_issue_log()
  out <- check_organisms(df, "infection (PE)", log)
  expect_identical(out$organism, c("E. coli", "Klebsiella"))
  expect_identical(out$v, c(1L, 3L))
  expect_length(log$get(), 1)
  expect_match(log$get(), "infection \\(PE\\) file, row 3: organism `Staph auerus`")
  expect_match(log$get(), "not in the accepted organism list")
})

test_that("check_organisms() names every unrecognised organism in a row", {
  log <- new_issue_log()
  check_organisms(data.frame(organism = "E. coli, banana, kiwi"), "PE", log)
  expect_length(log$get(), 1)
  expect_match(log$get(), "`banana`.*`kiwi`")
})

test_that("check_organisms() uses the stamped sheet row", {
  df <- data.frame(organism = c("E. coli", "banana"), .sheet_row = c(2L, 11L))
  log <- new_issue_log()
  check_organisms(df, "PE", log)
  expect_match(log$get(), "row 11:")
})

test_that("check_organisms() leaves blank cells alone and handles an empty frame", {
  df <- data.frame(organism = c(NA, "", "  ", "E. coli"))
  log <- new_issue_log()
  out <- check_organisms(df, "PE", log)
  expect_identical(nrow(out), 4L)
  expect_length(log$get(), 0)
  empty <- data.frame(organism = character(0))
  expect_identical(check_organisms(empty, "PE", new_issue_log()), empty)
})

test_that("check_organisms() requires the organism column", {
  expect_error(check_organisms(data.frame(a = 1), "PE", new_issue_log()))
})

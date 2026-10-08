# Tests for R/ingest_build.R: building pd_infection / pd_catheter / pd_patient objects

d <- function(x) as.Date(x)

# Catheter rows
raw_cath <- function(patient_id = "P1",
                     catheter_id = paste0(patient_id, "_01"),
                     insertion_date = d("2024-05-01"),
                     procedure_type = "Surgical",
                     pd_start_date = d("2024-06-01"),
                     pd_stop_date = as.Date(NA),
                     removal_reason = NA_character_) {
  tibble::tibble(patient_id = patient_id, catheter_id = catheter_id,
                 insertion_date = insertion_date, procedure_type = procedure_type,
                 pd_start_date = pd_start_date, pd_stop_date = pd_stop_date,
                 removal_reason = removal_reason)
}

# Peritonitis rows
raw_pe <- function(patient_id = "P1",
                   date_of_infection = d("2025-03-01"),
                   organism = "E. coli",
                   last_dose_antibiotic = date_of_infection + 14,
                   outcome = NA_character_,
                   outcome_date = as.Date(NA)) {
  df <- tibble::tibble(patient_id = patient_id, date_of_infection = date_of_infection,
                       last_dose_antibiotic = last_dose_antibiotic,
                       outcome = outcome, outcome_date = outcome_date)
  df$organism_list <- lapply(organism, function(o) as.list(o))
  df
}


# create_catheter_id()

test_that("create_catheter_id() numbers a patient's catheters by insertion date", {
  ids <- create_catheter_id(c("A", "A", "B"), d(c("2024-05-01", "2023-01-01", "2024-01-01")))
  expect_identical(ids, c("A_02", "A_01", "B_01"))
})

test_that("create_catheter_id() keeps the order of the input rows", {
  ids <- create_catheter_id(c("B", "A", "B"), d(c("2024-01-01", "2024-01-01", "2023-01-01")))
  expect_identical(ids, c("B_02", "A_01", "B_01"))
})

test_that("create_catheter_id() gives an undated catheter the highest sequence number", {
  ids <- create_catheter_id(c("A", "A"), c(as.Date(NA), d("2024-01-01")))
  expect_identical(ids, c("A_02", "A_01"))
})

test_that("create_catheter_id() returns NA for a missing patient_id", {
  ids <- create_catheter_id(c("A", NA), d(c("2024-01-01", "2024-01-01")))
  expect_identical(ids, c("A_01", NA))
})

test_that("create_catheter_id() pads the sequence to two digits", {
  ids <- create_catheter_id(rep("A", 10), d("2020-01-01") + 0:9)
  expect_identical(ids[c(1, 10)], c("A_01", "A_10"))
})

test_that("create_catheter_id() rejects bad input", {
  expect_error(create_catheter_id(c("A", "B"), d("2024-01-01")))
  expect_error(create_catheter_id("A", "2024-01-01"))
  expect_identical(create_catheter_id(character(0), as.Date(character(0))), character(0))
})


# match_active_catheter_id()

test_that("match_active_catheter_id() returns the catheter active on the infection date", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31")),
    raw_cath(catheter_id = "P1_02", pd_start_date = d("2025-01-15")))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P1", d("2024-06-01"), caths, log), "P1_01")
  expect_identical(match_active_catheter_id("P1", d("2025-06-01"), caths, log), "P1_02")
  expect_length(log$get(), 0)
})

test_that("match_active_catheter_id() includes the start and stop dates themselves", {
  caths <- raw_cath(pd_start_date = d("2025-01-01"), pd_stop_date = d("2025-02-01"))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P1", d("2025-01-01"), caths, log), "P1_01")
  expect_identical(match_active_catheter_id("P1", d("2025-02-01"), caths, log), "P1_01")
})

test_that("match_active_catheter_id() logs and returns NA when no catheter is active", {
  caths <- raw_cath(pd_start_date = d("2025-01-01"), pd_stop_date = d("2025-02-01"))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P1", d("2025-03-01"), caths, log),
                   NA_character_)
  expect_match(log$get(), "Patient P1: no active PD catheter on infection_date 2025-03-01")
})

test_that("match_active_catheter_id() ignores a catheter with no pd_start_date", {
  caths <- raw_cath(pd_start_date = as.Date(NA))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P1", d("2025-03-01"), caths, log),
                   NA_character_)
  expect_length(log$get(), 1)
})

test_that("match_active_catheter_id() logs overlapping catheters and uses the first", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", pd_start_date = d("2024-01-01")),
    raw_cath(catheter_id = "P1_02", pd_start_date = d("2024-06-01")))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P1", d("2025-01-01"), caths, log), "P1_01")
  expect_match(log$get(), "2 overlapping PD catheters .*P1_01, P1_02")
})

test_that("match_active_catheter_id() returns NA silently for a patient with no catheters", {
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P9", d("2025-01-01"), raw_cath(), log),
                   NA_character_)
  expect_length(log$get(), 0)
})

test_that("match_active_catheter_id() only looks at the named patient's catheters", {
  caths <- rbind(raw_cath("P1"), raw_cath("P2", pd_start_date = d("2025-02-01")))
  log <- new_issue_log()
  expect_identical(match_active_catheter_id("P2", d("2025-03-01"), caths, log), "P2_01")
})


# build_patient_infections()

test_that("build_patient_infections() builds one pd_infection per row", {
  df <- raw_pe(date_of_infection = d(c("2025-03-01", "2025-08-01")),
               organism = c("E. coli", "Klebsiella"))
  out <- build_patient_infections(df, new_issue_log())
  expect_length(out, 2)
  expect_true(all(vapply(out, inherits, logical(1), "pd_infection")))
  expect_identical(out[[2]]$infection_date, d("2025-08-01"))
})

test_that("build_patient_infections() chains each episode to the one before it", {
  # same organism, a few days after the last antibiotic dose -> relapsing
  df <- raw_pe(date_of_infection = d(c("2025-03-01", "2025-03-20")),
               organism = c("E. coli", "E. coli"),
               last_dose_antibiotic = d(c("2025-03-15", "2025-04-02")))
  out <- build_patient_infections(df, new_issue_log())
  expect_true(is.na(out[[1]]$episode_type))
  expect_identical(as.character(out[[2]]$episode_type), "relapsing")
})

test_that("build_patient_infections() carries outcome and outcome_date", {
  df <- raw_pe(outcome = "catheter removed", outcome_date = d("2025-03-05"))
  out <- build_patient_infections(df, new_issue_log())
  expect_identical(as.character(out[[1]]$outcome), "catheter removed")
  expect_identical(out[[1]]$outcome_date, d("2025-03-05"))
})

test_that("build_patient_infections() logs and skips an invalid episode, and keeps the rest", {
  df <- raw_pe(date_of_infection = d(c("2025-03-01", "2025-08-01")),
               organism = c("E. coli", "Klebsiella"))
  df$outcome_date[1] <- d("2025-03-05")   # an outcome_date with no outcome is rejected
  log <- new_issue_log()
  out <- build_patient_infections(df, log)
  expect_length(out, 1)
  expect_identical(out[[1]]$infection_date, d("2025-08-01"))
  expect_length(log$get(), 1)
  expect_match(log$get(), "Patient P1, episode on 2025-03-01: .*\\(episode skipped\\)")
})

test_that("build_patient_infections() returns an empty list for no rows", {
  expect_identical(build_patient_infections(raw_pe()[0, ], new_issue_log()), list())
})


# build_infections_by_catheter()

test_that("build_infections_by_catheter() groups episodes under the catheter active on their date", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31")),
    raw_cath(catheter_id = "P1_02", pd_start_date = d("2025-01-15")))
  pe <- raw_pe(date_of_infection = d(c("2024-06-01", "2025-03-01", "2025-09-01")),
               organism = c("E. coli", "Klebsiella", "Pseudomonas"))
  out <- build_infections_by_catheter(pe, caths, new_issue_log())
  expect_named(out, c("P1_01", "P1_02"))
  expect_length(out$P1_01, 1)
  expect_length(out$P1_02, 2)
})

test_that("build_infections_by_catheter() groups across patients", {
  caths <- rbind(raw_cath("P1"), raw_cath("P2"))
  pe <- rbind(raw_pe("P1"), raw_pe("P2", organism = "Klebsiella"))
  out <- build_infections_by_catheter(pe, caths, new_issue_log())
  expect_named(out, c("P1_01", "P2_01"))
  expect_identical(out$P2_01[[1]]$patient_id, "P2")
})

test_that("build_infections_by_catheter() logs and drops an episode with no active catheter", {
  caths <- raw_cath(pd_start_date = d("2025-06-01"))
  pe <- raw_pe(date_of_infection = d("2025-03-01"))
  log <- new_issue_log()
  out <- build_infections_by_catheter(pe, caths, log)
  expect_length(out, 0)
  expect_match(log$get(), "no active PD catheter")
})

test_that("build_infections_by_catheter() drops an episode of a patient with no catheter at all", {
  out <- build_infections_by_catheter(raw_pe("P9"), raw_cath("P1"), new_issue_log())
  expect_length(out, 0)
})

test_that("build_infections_by_catheter() returns an empty list for no episodes", {
  expect_identical(build_infections_by_catheter(raw_pe()[0, ], raw_cath(), new_issue_log()),
                   list())
})

test_that("build_infections_by_catheter() leaves out an episode that failed validation", {
  pe <- raw_pe(date_of_infection = d(c("2025-03-01", "2025-08-01")),
               organism = c("E. coli", "Klebsiella"))
  pe$outcome_date[1] <- d("2025-03-05")   # an outcome_date with no outcome is rejected
  log <- new_issue_log()
  out <- build_infections_by_catheter(pe, raw_cath(), log)
  expect_length(out$P1_01, 1)
  expect_match(log$get(), "episode skipped")
})


# build_patient_catheters()

test_that("build_patient_catheters() builds the patient's catheters and attaches their episodes", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", insertion_date = d("2022-12-01"),
             pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31")),
    raw_cath(catheter_id = "P1_02", insertion_date = d("2025-01-01"),
             pd_start_date = d("2025-01-15")),
    raw_cath("P2"))
  by_cath <- build_infections_by_catheter(
    raw_pe(date_of_infection = d("2025-03-01")), caths, new_issue_log())
  out <- build_patient_catheters("P1", caths, by_cath, T0, T1, new_issue_log())
  expect_length(out, 2)
  expect_identical(vapply(out, function(z) z$catheter_id, character(1)), c("P1_01", "P1_02"))
  expect_length(out[[1]]$infections, 0)
  expect_length(out[[2]]$infections, 1)
})

test_that("build_patient_catheters() scopes the episode count to the reporting period", {
  caths <- raw_cath()
  by_cath <- build_infections_by_catheter(
    raw_pe(date_of_infection = d(c("2024-09-01", "2025-03-01")), organism = c("A", "B")),
    caths, new_issue_log())
  out <- build_patient_catheters("P1", caths, by_cath, T0, T1, new_issue_log())
  expect_length(out[[1]]$infections, 2)
  expect_equal(out[[1]]$n_peritonitis_episodes, 1)
  expect_true(out[[1]]$peritonitis_flag)
})

test_that("build_patient_catheters() logs and skips a catheter that fails validation", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", pd_start_date = d("2024-06-01")),
    raw_cath(catheter_id = "P1_02", insertion_date = d("2025-01-01"),
             pd_start_date = d("2025-02-01"), pd_stop_date = d("2025-01-01")))
  log <- new_issue_log()
  out <- build_patient_catheters("P1", caths, list(), T0, T1, log)
  expect_length(out, 1)
  expect_identical(out[[1]]$catheter_id, "P1_01")
  expect_match(log$get(), "Catheter P1_02: .*\\(catheter skipped\\)")
})

test_that("build_patient_catheters() returns an empty list for an unknown patient", {
  expect_identical(build_patient_catheters("P9", raw_cath(), list(), T0, T1, new_issue_log()),
                   list())
})


# build_patient_list()

# a one-patient setup helper: catheters, patients and taus for the given ids
bpl_inputs <- function(pids = "P1", caths = NULL, patients = NULL, taus = NULL) {
  if (is.null(caths)) caths <- do.call(rbind, lapply(pids, raw_cath))
  if (is.null(patients)) {
    patients <- tibble::tibble(patient_id = pids, gender = "Female",
                               date_of_birth = d("1970-01-01"),
                               dialysis_type = "APD")
  }
  if (is.null(taus)) {
    taus <- stats::setNames(lapply(pids, function(p) {
      list(reason = NA_character_, date = as.Date(NA), detail = NA_character_)
    }), pids)
  }
  list(pids = pids, taus = taus, raw_catheters = caths, raw_patients = patients)
}

run_bpl <- function(inp, by_cath = list(), log = new_issue_log()) {
  build_patient_list(inp$pids, inp$taus, inp$raw_catheters, inp$raw_patients,
                     by_cath, T0, T1, log)
}

test_that("build_patient_list() returns the two pieces pd_unit() needs", {
  out <- run_bpl(bpl_inputs())
  expect_named(out, c("patient_list", "transfer_details"))
  expect_length(out$patient_list, 1)
  expect_s3_class(out$patient_list[[1]], "pd_patient")
})

test_that("build_patient_list() passes demographics and the window to each patient", {
  p <- run_bpl(bpl_inputs())$patient_list[[1]]
  expect_identical(p$gender, "Female")
  expect_identical(p$date_of_birth, d("1970-01-01"))
  expect_identical(p$dialysis_type, "APD")
  expect_identical(p$t0, T0)
  expect_identical(p$t1, T1)
  expect_identical(p$n_catheters, 1L)
})

test_that("build_patient_list() records each patient's tau as transfer_reason / transfer_date / detail", {
  inp <- bpl_inputs(caths = raw_cath(pd_stop_date = d("2025-06-30")))
  inp$taus$P1 <- list(reason = "death", date = d("2025-06-30"), detail = "Cardiac")
  out <- run_bpl(inp)
  expect_identical(out$patient_list[[1]]$transfer_reason, "death")
  expect_identical(out$patient_list[[1]]$transfer_date, d("2025-06-30"))
  expect_identical(out$transfer_details, c(P1 = "Cardiac"))
})

test_that("build_patient_list() stores NA when a patient has no tau detail", {
  out <- run_bpl(bpl_inputs())
  expect_identical(out$transfer_details, c(P1 = NA_character_))
})

test_that("build_patient_list() leaves out patients who were not on PD during the period", {
  caths <- rbind(raw_cath("P1"),
                 raw_cath("P2", pd_start_date = d("2022-01-01"), pd_stop_date = d("2023-01-01")),
                 raw_cath("P3", pd_start_date = d("2026-03-01")))
  out <- run_bpl(bpl_inputs(c("P1", "P2", "P3"), caths = caths))
  expect_identical(vapply(out$patient_list, function(p) p$patient_id, character(1)), "P1")
  expect_identical(names(out$transfer_details), "P1")
})

test_that("build_patient_list() logs and skips a patient whose open catheter outlives their tau", {
  # pd_unit() closes such catheters at tau before this point; this is the safety net
  inp <- bpl_inputs()
  inp$taus$P1 <- list(reason = "death", date = d("2025-06-30"), detail = NA_character_)
  log <- new_issue_log()
  out <- run_bpl(inp, log = log)
  expect_length(out$patient_list, 0)
  expect_match(log$get()[1], "Patient P1: .*\\(patient skipped\\)")
})

test_that("build_patient_list() leaves out a patient whose tau falls before the period", {
  inp <- bpl_inputs(caths = raw_cath(pd_stop_date = d("2024-06-30")))
  inp$taus$P1 <- list(reason = "death", date = d("2024-06-30"), detail = NA_character_)
  expect_length(run_bpl(inp)$patient_list, 0)
})

test_that("build_patient_list() keeps patient_list in the order of pids", {
  out <- run_bpl(bpl_inputs(c("P2", "P1")))
  expect_identical(vapply(out$patient_list, function(p) p$patient_id, character(1)),
                   c("P2", "P1"))
})

test_that("build_patient_list() attaches catheters and their episodes to the patient", {
  inp <- bpl_inputs()
  by_cath <- build_infections_by_catheter(raw_pe(), inp$raw_catheters, new_issue_log())
  p <- run_bpl(inp, by_cath)$patient_list[[1]]
  expect_equal(p$n_episodes, 1)
  expect_length(p$catheters[[1]]$infections, 1)
})

test_that("build_patient_list() logs a patient whose every catheter fails validation", {
  caths <- raw_cath(insertion_date = d("2025-01-01"), pd_start_date = d("2025-02-01"),
                    pd_stop_date = d("2025-01-01"))
  # a stop date before the start date makes pd_catheter() reject the only catheter
  log <- new_issue_log()
  out <- run_bpl(bpl_inputs(caths = caths), log = log)
  expect_length(out$patient_list, 1)   # still built; the logged issue stops pd_unit()
  expect_true(any(grepl("catheter skipped", log$get())))
  expect_true(any(grepl("Patient P1: no valid PD catheter remains", log$get())))
})

test_that("build_patient_list() copes with a patient missing from raw_patients", {
  inp <- bpl_inputs()
  inp$raw_patients <- inp$raw_patients[0, ]
  p <- run_bpl(inp)$patient_list[[1]]
  expect_true(is.na(p$gender))
  expect_true(is.na(p$date_of_birth))
})

test_that("build_patient_list() returns empty results for no patients", {
  out <- build_patient_list(character(0), list(), raw_cath()[0, ], tibble::tibble(),
                            list(), T0, T1, new_issue_log())
  expect_identical(out$patient_list, list())
  expect_identical(out$transfer_details, character(0))
})


# patient_demo_value() / patient_dob_value()

test_that("patient_demo_value() reads and trims a value", {
  demo <- data.frame(gender = "  Female ", stringsAsFactors = FALSE)
  expect_identical(patient_demo_value(demo, "gender"), "Female")
})

test_that("patient_demo_value() returns NA for blank, NA, a missing column or a missing row", {
  demo <- data.frame(a = c(NA, ""), b = " ", stringsAsFactors = FALSE)
  expect_identical(patient_demo_value(demo, "a"), NA_character_)
  expect_identical(patient_demo_value(demo[2, ], "a"), NA_character_)
  expect_identical(patient_demo_value(demo, "b"), NA_character_)
  expect_identical(patient_demo_value(demo, "no_such_col"), NA_character_)
  expect_identical(patient_demo_value(demo[0, ], "a"), NA_character_)
})

test_that("patient_demo_value() returns character even for a numeric column", {
  expect_identical(patient_demo_value(data.frame(height = 170), "height"), "170")
})

test_that("patient_dob_value() keeps the Date class", {
  demo <- data.frame(date_of_birth = d("1970-01-01"))
  expect_identical(patient_dob_value(demo), d("1970-01-01"))
})

test_that("patient_dob_value() returns a Date NA for NA, a missing column or a missing row", {
  expect_identical(patient_dob_value(data.frame(date_of_birth = as.Date(NA))), as.Date(NA))
  expect_identical(patient_dob_value(data.frame(x = 1)), as.Date(NA))
  expect_identical(patient_dob_value(data.frame(date_of_birth = as.Date(character(0)))), as.Date(NA))
})

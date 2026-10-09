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

# Patient rows
raw_patient <- function(patient_id = "P1",
                        gender = "Female",
                        date_of_birth = d("1970-01-01"),
                        dialysis_type = "APD") {
  tibble::tibble(patient_id = patient_id, gender = gender,
                 date_of_birth = date_of_birth, dialysis_type = dialysis_type)
}

# A patient's tau (no transfer by default)
raw_tau <- function(reason = NA_character_,
                    date = as.Date(NA),
                    detail = NA_character_) {
  list(reason = reason, date = date, detail = detail)
}


# create_catheter_id()

test_that("create_catheter_id() numbers a patient's catheters by insertion date", {
  ids <- create_catheter_id(c("A", "A", "B"), d(c("2024-05-01", "2023-01-01", "2024-01-01")))
  expect_identical(ids, c("A_02", "A_01", "B_01"))
})


test_that("create_catheter_id() gives an undated catheter the highest sequence number", {
  ids <- create_catheter_id(c("A", "A"), c(as.Date(NA), d("2024-01-01")))
  expect_identical(ids, c("A_02", "A_01"))
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


# build_patient_infections()

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
  expect_match(log$get(), "Patient P1, episode on 2025-03-01: .*\\(episode not checked further")
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


test_that("build_infections_by_catheter() returns an empty list for no episodes", {
  expect_identical(build_infections_by_catheter(raw_pe()[0, ], raw_cath(), new_issue_log()),
                   list())
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


test_that("build_patient_catheters() returns an empty list for an unknown patient", {
  expect_identical(build_patient_catheters("P9", raw_cath(), list(), T0, T1, new_issue_log()),
                   list())
})


test_that("build_patient_catheters() fills total_exposure_days, inclusive of both endpoints", {
  # active before the period: all 365 days of 2025
  out <- build_patient_catheters("P1", raw_cath(), list(), T0, T1, new_issue_log())
  expect_equal(out[[1]]$total_exposure_days, 365)

  # starts and stops inside the period: 10-19 Feb is 10 days
  caths <- raw_cath(pd_start_date = d("2025-02-10"), pd_stop_date = d("2025-02-19"),
                    insertion_date = d("2025-01-20"))
  out <- build_patient_catheters("P1", caths, list(), T0, T1, new_issue_log())
  expect_equal(out[[1]]$total_exposure_days, 10)

  # window entirely before the period: no exposure in it
  caths <- raw_cath(pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31"))
  out <- build_patient_catheters("P1", caths, list(), T0, T1, new_issue_log())
  expect_equal(out[[1]]$total_exposure_days, 0)
})

test_that("build_patient_catheters() censors total_exposure_days at the patient's tau", {
  # still-active catheter, patient left PD on 31 Mar: 1 Jan - 31 Mar 2025 = 90 days
  out <- build_patient_catheters("P1", raw_cath(), list(), T0, T1, new_issue_log(),
                                 tau = d("2025-03-31"))
  expect_equal(out[[1]]$total_exposure_days, 90)
})

test_that("a catheter's total_exposure_days matches exposure_days_in_period on the catheters tibble", {
  caths <- rbind(
    raw_cath(catheter_id = "P1_01", insertion_date = d("2022-12-01"),
             pd_start_date = d("2023-01-01"), pd_stop_date = d("2024-12-31")),
    raw_cath(catheter_id = "P1_02", insertion_date = d("2025-01-01"),
             pd_start_date = d("2025-01-15")))
  out <- build_patient_catheters("P1", caths, list(), T0, T1, new_issue_log())
  pat <- pd_patient(patient_id = "P1", catheters = out, t0 = T0, t1 = T1)
  tbl <- catheters_to_tibble(list(pat), T0, T1)
  expect_equal(unlist(lapply(out, function(z) z$total_exposure_days)),
               tbl$exposure_days_in_period)
  expect_equal(tbl$exposure_days_in_period, c(0, 351))   # 15 Jan - 31 Dec 2025 inclusive
})



# build_patient_list()

test_that("build_patient_list() passes demographics and the window to each patient", {
  out <- build_patient_list("P1", list(P1 = raw_tau()), raw_cath(), raw_patient(),
                            list(), T0, T1, new_issue_log())

  p <- out$patient_list[[1]]
  expect_identical(p$gender, "Female")
  expect_identical(p$date_of_birth, d("1970-01-01"))
  expect_identical(p$dialysis_type, "APD")
  expect_identical(p$t0, T0)
  expect_identical(p$t1, T1)
  expect_identical(p$n_catheters, 1L)
})

test_that("build_patient_list() records each patient's tau as transfer_reason / transfer_date / detail", {
  caths <- raw_cath(pd_stop_date = d("2025-06-30"))
  taus <- list(P1 = raw_tau("death", d("2025-06-30"), "Cardiac"))

  out <- build_patient_list("P1", taus, caths, raw_patient(),
                            list(), T0, T1, new_issue_log())

  expect_identical(out$patient_list[[1]]$transfer_reason, "death")
  expect_identical(out$patient_list[[1]]$transfer_date, d("2025-06-30"))
  expect_identical(out$transfer_details, c(P1 = "Cardiac"))
})


test_that("build_patient_list() leaves out patients who were not on PD during the period", {
  pids <- c("P1", "P2", "P3")
  caths <- rbind(raw_cath("P1"),
                 raw_cath("P2", pd_start_date = d("2022-01-01"), pd_stop_date = d("2023-01-01")),
                 raw_cath("P3", pd_start_date = d("2026-03-01")))
  taus <- list(P1 = raw_tau(), P2 = raw_tau(), P3 = raw_tau())

  out <- build_patient_list(pids, taus, caths, raw_patient(pids),
                            list(), T0, T1, new_issue_log())

  expect_identical(vapply(out$patient_list, function(p) p$patient_id, character(1)), "P1")
  expect_identical(names(out$transfer_details), "P1")
})


test_that("build_patient_list() logs a patient whose every catheter fails validation", {
  # a stop date before the start date makes pd_catheter() reject the only catheter
  caths <- raw_cath(insertion_date = d("2025-01-01"), pd_start_date = d("2025-02-01"),
                    pd_stop_date = d("2025-01-01"))
  log <- new_issue_log()

  out <- build_patient_list("P1", list(P1 = raw_tau()), caths, raw_patient(),
                            list(), T0, T1, log)

  expect_length(out$patient_list, 1)   # still built; the logged issue stops pd_unit()
  expect_true(any(grepl("catheter not checked further", log$get())))
  expect_true(any(grepl("Patient P1: no valid PD catheter remains", log$get())))
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

test_that("patient_dob_value() keeps the Date class", {
  demo <- data.frame(date_of_birth = d("1970-01-01"))
  expect_identical(patient_dob_value(demo), d("1970-01-01"))
})

test_that("patient_dob_value() returns a Date NA for NA, a missing column or a missing row", {
  expect_identical(patient_dob_value(data.frame(date_of_birth = as.Date(NA))), as.Date(NA))
  expect_identical(patient_dob_value(data.frame(x = 1)), as.Date(NA))
  expect_identical(patient_dob_value(data.frame(date_of_birth = as.Date(character(0)))), as.Date(NA))
})

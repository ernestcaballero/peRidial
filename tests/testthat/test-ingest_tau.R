# Tests for R/ingest_tau.R: deriving the censoring point (tau) of a patient's PD

d <- function(x) as.Date(x)

# One patient's A3 demographic row; every censoring field blank unless supplied
tau_demo <- function(date_of_death = as.Date(NA), cause_of_death = NA_character_,
                     transplant_date = as.Date(NA)) {
  data.frame(patient_id = "P1", date_of_death = date_of_death,
             cause_of_death = cause_of_death, transplant_date = transplant_date)
}

# Modality-change rows for the patient (none by default)
tau_mod <- function(change = character(0), date = as.Date(character(0)),
                    reason = rep(NA_character_, length(change))) {
  data.frame(dialysis_modality_change = change, date_modality_change = date,
             modality_change_reason = reason, stringsAsFactors = FALSE)
}

# Catheter rows (one open catheter by default)
tau_cath <- function(pd_start_date = d("2024-01-01"), pd_stop_date = as.Date(NA),
                     removal_reason = NA_character_) {
  data.frame(pd_start_date = pd_start_date, pd_stop_date = pd_stop_date,
             removal_reason = removal_reason, stringsAsFactors = FALSE)
}


# derive_patient_tau(): each rule on its own

test_that("derive_patient_tau() returns NA when nothing ends PD", {
  tau <- derive_patient_tau(tau_demo(), tau_mod(), tau_cath())
  expect_true(is.na(tau$reason))
  expect_true(is.na(tau$date))
  expect_true(is.na(tau$detail))
  expect_false(tau$transplant_gap)
  expect_false(tau$hd_transfer_gap)
})

test_that("derive_patient_tau() censors at death and carries the cause as detail", {
  tau <- derive_patient_tau(
    tau_demo(date_of_death = d("2025-06-01"), cause_of_death = "Cardiac"),
    tau_mod(), tau_cath())
  expect_identical(tau$reason, "death")
  expect_identical(tau$date, d("2025-06-01"))
  expect_identical(tau$detail, "Cardiac")
})

test_that("derive_patient_tau() copes with a demo frame that has no cause_of_death column", {
  demo <- data.frame(patient_id = "P1", date_of_death = d("2025-06-01"),
                     transplant_date = as.Date(NA))
  tau <- derive_patient_tau(demo, tau_mod(), tau_cath())
  expect_identical(tau$reason, "death")
  expect_true(is.na(tau$detail))
})

test_that("derive_patient_tau() censors at a transplant date", {
  tau <- derive_patient_tau(tau_demo(transplant_date = d("2025-04-01")),
                            tau_mod(), tau_cath())
  expect_identical(tau$reason, "transplant")
  expect_identical(tau$date, d("2025-04-01"))
})

test_that("derive_patient_tau() censors at an 'Any PD to HD' change, with its reason", {
  mod <- tau_mod("Any PD to HD", d("2025-03-01"), "Peritonitis")
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_identical(tau$reason, "permanent transfer to HD")
  expect_identical(tau$date, d("2025-03-01"))
  expect_identical(tau$detail, "Peritonitis")
})

test_that("derive_patient_tau() matches the PD-to-HD label regardless of case and padding", {
  mod <- tau_mod("  ANY PD TO HD ", d("2025-03-01"), "x")
  expect_identical(derive_patient_tau(tau_demo(), mod, tau_cath())$reason,
                   "permanent transfer to HD")
})

test_that("derive_patient_tau() ignores modality changes that stay within PD", {
  mod <- tau_mod(c("CAPD to APD", "HD to any PD"), d(c("2025-02-01", "2025-03-01")))
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_true(is.na(tau$reason))
})

test_that("derive_patient_tau() uses the LAST dated 'Any PD to HD' row when a patient has several", {
  mod <- tau_mod(rep("Any PD to HD", 2), d(c("2025-02-01", "2025-05-01")), c("first", "second"))
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_identical(tau$date, d("2025-05-01"))
  expect_identical(tau$detail, "second")
})


# derive_patient_tau(): earliest event wins

test_that("derive_patient_tau() picks the earliest of death, transplant and PD-to-HD", {
  demo <- tau_demo(date_of_death = d("2025-09-01"), transplant_date = d("2025-07-01"))
  mod <- tau_mod("Any PD to HD", d("2025-05-01"), "Failure")
  tau <- derive_patient_tau(demo, mod, tau_cath())
  expect_identical(tau$reason, "permanent transfer to HD")
  expect_identical(tau$date, d("2025-05-01"))

  mod2 <- tau_mod("Any PD to HD", d("2025-08-01"), "Failure")
  tau2 <- derive_patient_tau(demo, mod2, tau_cath())
  expect_identical(tau2$reason, "transplant")
  expect_identical(tau2$date, d("2025-07-01"))
})

test_that("derive_patient_tau() takes an explicit event over the 'PD stopped' rule", {
  demo <- tau_demo(date_of_death = d("2025-09-01"))
  cath <- tau_cath(pd_stop_date = d("2025-03-01"))
  tau <- derive_patient_tau(demo, tau_mod(), cath)
  expect_identical(tau$reason, "death")
})


# derive_patient_tau(): PD stopped with no successor catheter

test_that("derive_patient_tau() censors at the last pd_stop_date when every catheter has closed", {
  cath <- tau_cath(pd_start_date = d(c("2023-01-01", "2024-01-01")),
                   pd_stop_date = d(c("2023-12-01", "2025-02-01")),
                   removal_reason = c("Infection", "Patient choice"))
  tau <- derive_patient_tau(tau_demo(), tau_mod(), cath)
  expect_identical(tau$reason, "pd stopped")
  expect_identical(tau$date, d("2025-02-01"))
  expect_identical(tau$detail, "Patient choice")
})

test_that("derive_patient_tau() reports a catheter removed for transplant as a transplant, not 'pd stopped'", {
  cath <- tau_cath(pd_stop_date = d("2025-02-01"), removal_reason = "Transplant")
  tau <- derive_patient_tau(tau_demo(), tau_mod(), cath)
  expect_identical(tau$reason, "transplant")
  expect_identical(tau$date, d("2025-02-01"))
})

test_that("derive_patient_tau() does not censor on a stop date while another catheter is still open", {
  cath <- tau_cath(pd_start_date = d(c("2023-01-01", "2024-01-01")),
                   pd_stop_date = c(d("2023-12-01"), as.Date(NA)))
  tau <- derive_patient_tau(tau_demo(), tau_mod(), cath)
  expect_true(is.na(tau$reason))
})

test_that("derive_patient_tau(censor_on_last_stop = FALSE) leaves a stopped patient uncensored", {
  cath <- tau_cath(pd_stop_date = d("2025-02-01"))
  tau <- derive_patient_tau(tau_demo(), tau_mod(), cath, censor_on_last_stop = FALSE)
  expect_true(is.na(tau$reason))
  expect_true(is.na(tau$date))
})

test_that("derive_patient_tau() copes with a catheter frame without removal_reason", {
  cath <- data.frame(pd_start_date = d("2024-01-01"), pd_stop_date = d("2025-02-01"))
  tau <- derive_patient_tau(tau_demo(), tau_mod(), cath)
  expect_identical(tau$reason, "pd stopped")
  expect_true(is.na(tau$detail))
})

test_that("derive_patient_tau() copes with a patient who has no catheter rows", {
  tau <- derive_patient_tau(tau_demo(), tau_mod(), tau_cath()[0, ])
  expect_true(is.na(tau$reason))
})


# derive_patient_tau(): data gaps

test_that("derive_patient_tau() flags a transplant change with no date anywhere", {
  mod <- tau_mod("Transplant", as.Date(NA))
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_true(tau$transplant_gap)
  expect_true(is.na(tau$reason))
})

test_that("derive_patient_tau() does not flag a transplant gap when the date is found", {
  mod <- tau_mod("Transplant", as.Date(NA))
  tau <- derive_patient_tau(tau_demo(transplant_date = d("2025-04-01")), mod, tau_cath())
  expect_false(tau$transplant_gap)
  expect_identical(tau$reason, "transplant")
})

test_that("derive_patient_tau() flags an 'Any PD to HD' change with no usable date", {
  mod <- tau_mod("Any PD to HD", as.Date(NA), "Failure")
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_true(tau$hd_transfer_gap)
  expect_true(is.na(tau$reason))
})

test_that("derive_patient_tau() does not flag the HD gap when another PD-to-HD row has a date", {
  mod <- tau_mod(rep("Any PD to HD", 2), c(as.Date(NA), d("2025-05-01")))
  tau <- derive_patient_tau(tau_demo(), mod, tau_cath())
  expect_false(tau$hd_transfer_gap)
  expect_identical(tau$date, d("2025-05-01"))
})


# find_transplant_date()

test_that("find_transplant_date() prefers the A3 transplant_date", {
  cath <- tau_cath(pd_stop_date = d("2025-01-01"), removal_reason = "Transplant")
  expect_identical(find_transplant_date(tau_demo(transplant_date = d("2025-04-01")), cath),
                   d("2025-04-01"))
})

test_that("find_transplant_date() falls back to a catheter removed for transplant", {
  cath <- tau_cath(pd_start_date = d(c("2023-01-01", "2024-01-01")),
                   pd_stop_date = d(c("2023-12-01", "2025-02-01")),
                   removal_reason = c("Infection", "Transplanted"))
  expect_identical(find_transplant_date(tau_demo(), cath), d("2025-02-01"))
})

test_that("find_transplant_date() takes the earliest matching stop date", {
  cath <- tau_cath(pd_start_date = d(c("2023-01-01", "2024-01-01")),
                   pd_stop_date = d(c("2023-12-01", "2025-02-01")),
                   removal_reason = c("transplant", "TRANSPLANT"))
  expect_identical(find_transplant_date(tau_demo(), cath), d("2023-12-01"))
})

test_that("find_transplant_date() ignores a transplant removal reason with no stop date", {
  cath <- tau_cath(pd_stop_date = as.Date(NA), removal_reason = "Transplant")
  expect_true(is.na(find_transplant_date(tau_demo(), cath)))
})

test_that("find_transplant_date() returns NA when there is nothing to go on", {
  expect_true(is.na(find_transplant_date(tau_demo(), tau_cath())))
  expect_true(is.na(find_transplant_date(tau_demo(), tau_cath()[0, ])))
})

test_that("find_transplant_date() returns a Date even when transplant_date arrives as POSIXct", {
  demo <- tau_demo()
  demo$transplant_date <- as.POSIXct("2025-04-01", tz = "UTC")
  out <- find_transplant_date(demo, tau_cath())
  expect_s3_class(out, "Date")
  expect_identical(out, d("2025-04-01"))
})


# on_pd_in_period()

test_that("on_pd_in_period() is FALSE with no catheters", {
  expect_false(on_pd_in_period(tau_cath()[0, ], T0, T1))
})

test_that("on_pd_in_period() is TRUE for a catheter active across the period", {
  expect_true(on_pd_in_period(tau_cath(), T0, T1))
})

test_that("on_pd_in_period() is TRUE for a catheter that overlaps either end of the period", {
  expect_true(on_pd_in_period(tau_cath(pd_start_date = d("2024-01-01"),
                                       pd_stop_date = d("2025-01-01")), T0, T1))
  expect_true(on_pd_in_period(tau_cath(pd_start_date = d("2025-12-31")), T0, T1))
})

test_that("on_pd_in_period() is FALSE for a catheter that ended before, or started after, the period", {
  expect_false(on_pd_in_period(tau_cath(pd_start_date = d("2023-01-01"),
                                        pd_stop_date = d("2024-12-31")), T0, T1))
  expect_false(on_pd_in_period(tau_cath(pd_start_date = d("2026-01-01")), T0, T1))
})

test_that("on_pd_in_period() ignores a catheter with no pd_start_date", {
  expect_false(on_pd_in_period(tau_cath(pd_start_date = as.Date(NA)), T0, T1))
})

test_that("on_pd_in_period() uses tau to cut a catheter short", {
  cath <- tau_cath(pd_start_date = d("2023-01-01"))
  expect_false(on_pd_in_period(cath, T0, T1, tau = d("2024-06-01")))
  expect_true(on_pd_in_period(cath, T0, T1, tau = d("2025-01-01")))
})

test_that("on_pd_in_period() treats a catheter running past tau as ending at tau", {
  cath <- tau_cath(pd_start_date = d("2023-01-01"), pd_stop_date = d("2025-06-01"))
  expect_false(on_pd_in_period(cath, T0, T1, tau = d("2024-12-31")))
})

# Tests for R/pd_unit_summary.R: the timing measures in summary.pd_unit()'s catheter block.
# summarise_catheters() only reads x$patients / x$catheters / x$infections, so
# the edge cases below use a plain list of tibbles rather than a full pd_unit.

d <- function(x) as.Date(x)

timing_unit <- function(patients, catheters, infections) {
  list(patients = tibble::as_tibble(patients),
       catheters = tibble::as_tibble(catheters),
       infections = tibble::as_tibble(infections))
}

# Two incident patients (I1, I2), one prevalent (P1)
#   I1: first catheter inserted 2025-02-01, PD start 2025-02-11 (10 days); a second catheter later.
#       Episodes 2025-03-13 (31 days after first PD start) and 2025-09-01.
#   I2: inserted 2025-04-01, PD start 2025-04-21 (20 days); no episodes.
#   P1: prevalent, inserted 2024-01-01, PD start 2024-03-01 (60 days); an episode on 2025-01-10.
timing_fixture <- function() {
  timing_unit(
    patients = data.frame(patient_id = c("I1", "I2", "P1"),
                          new_patient_flag = c(TRUE, TRUE, FALSE)),
    catheters = data.frame(
      patient_id = c("I1", "I1", "I2", "P1"),
      catheter_id = c("I1_01", "I1_02", "I2_01", "P1_01"),
      insertion_date = d(c("2025-02-01", "2025-08-01", "2025-04-01", "2024-01-01")),
      pd_start_date = d(c("2025-02-11", "2025-08-10", "2025-04-21", "2024-03-01"))),
    infections = data.frame(
      patient_id = c("I1", "I1", "P1"),
      catheter_id = c("I1_01", "I1_02", "P1_01"),
      infection_date = d(c("2025-03-13", "2025-09-01", "2025-01-10"))))
}


test_that("time to PD start from insertion is the mean over incident patients' first catheters", {
  out <- summarise_catheters(timing_fixture())$time_to_pd_start_days
  # (10 + 20) / 2; the second catheter of I1 and the prevalent patient are not used
  expect_equal(out[["mean"]], 15)
  expect_equal(out[["n"]], 2)
  expect_named(out, c("mean", "n"))
})

test_that("time to first peritonitis episode is PD start to the first episode, among incident patients with one", {
  out <- summarise_catheters(timing_fixture())$time_to_first_peritonitis_days
  # only I1 has an episode: 2025-02-11 -> 2025-03-13 = 30 days
  expect_equal(out[["mean"]], as.numeric(d("2025-03-13") - d("2025-02-11")))
  expect_equal(out[["n"]], 1)
})

test_that("the timing measures are means, not medians", {
  x <- timing_fixture()
  x$patients <- rbind(x$patients, data.frame(patient_id = "I3", new_patient_flag = TRUE))
  x$catheters <- rbind(x$catheters, data.frame(
    patient_id = "I3", catheter_id = "I3_01",
    insertion_date = d("2025-05-01"), pd_start_date = d("2025-07-30")))  # 90 days
  out <- summarise_catheters(x)$time_to_pd_start_days
  expect_equal(out[["mean"]], (10 + 20 + 90) / 3)   # the median would be 20
})

test_that("a patient's first catheter is the one with the earliest PD start, whatever the row order", {
  x <- timing_fixture()
  x$catheters <- x$catheters[c(2, 1, 3, 4), ]
  expect_equal(summarise_catheters(x)$time_to_pd_start_days[["mean"]], 15)
})

test_that("a catheter with no PD start date is ignored", {
  x <- timing_fixture()
  x$catheters$pd_start_date[x$catheters$catheter_id == "I2_01"] <- as.Date(NA)
  out <- summarise_catheters(x)$time_to_pd_start_days
  expect_equal(out[["mean"]], 10)
  expect_equal(out[["n"]], 1)
})

test_that("a missing insertion date drops that patient from the mean rather than breaking it", {
  x <- timing_fixture()
  x$catheters$insertion_date[x$catheters$catheter_id == "I1_01"] <- as.Date(NA)
  out <- summarise_catheters(x)$time_to_pd_start_days
  expect_equal(out[["mean"]], 20)
  expect_equal(out[["n"]], 1)
})

test_that("with no incident patients both measures are NA with n = 0", {
  x <- timing_fixture()
  x$patients$new_patient_flag <- FALSE
  out <- summarise_catheters(x)
  expect_true(is.na(out$time_to_pd_start_days[["mean"]]))
  expect_equal(out$time_to_pd_start_days[["n"]], 0)
  expect_true(is.na(out$time_to_first_peritonitis_days[["mean"]]))
  expect_equal(out$time_to_first_peritonitis_days[["n"]], 0)
})

test_that("with no episodes the time to first peritonitis is NA but the PD-start measure still works", {
  x <- timing_fixture()
  x$infections <- x$infections[0, ]
  out <- summarise_catheters(x)
  expect_true(is.na(out$time_to_first_peritonitis_days[["mean"]]))
  expect_equal(out$time_to_pd_start_days[["mean"]], 15)
})

test_that("the timing measures cope with tables that lack the columns they need", {
  x <- timing_fixture()
  x$patients$new_patient_flag <- NULL
  out <- summarise_catheters(x)
  expect_true(is.na(out$time_to_pd_start_days[["mean"]]))
  expect_true(is.na(out$time_to_first_peritonitis_days[["mean"]]))

  y <- timing_fixture()
  y$catheters$insertion_date <- NULL
  expect_true(is.na(summarise_catheters(y)$time_to_pd_start_days[["mean"]]))
  expect_equal(summarise_catheters(y)$time_to_first_peritonitis_days[["n"]], 1)
})

test_that("summarise_catheters() still reports the catheter count and procedure types", {
  x <- timing_fixture()
  x$catheters$procedure_type <- c("Surgical", NA, "Surgical", "Percutaneous")
  out <- summarise_catheters(x)
  expect_equal(out$n_catheters, 4)
  expect_equal(as.list(out$procedure_type),
               list(Percutaneous = 1L, Surgical = 2L, unknown = 1L))
})


# summary.pd_unit() output ---------------------------------------------------------

test_that("summary.pd_unit() labels the two timing lines as means and drops the old quartile lines", {
  txt <- capture.output(summary(make_subset_unit()))
  expect_true(any(grepl("Time to PD start from insertion (mean)", txt, fixed = TRUE)))
  expect_true(any(grepl("Time to first peritonitis episode (mean)", txt, fixed = TRUE)))
  expect_false(any(grepl("Insertion to PD start|PD start to infection|Q1/median/Q3", txt)))
})

test_that("summary.pd_unit() prints the mean days, without a patient count", {
  # CCC0003 is the only incident patient: inserted 2025-02-01, PD start 2025-02-15,
  # first episode 2025-04-01
  txt <- capture.output(res <- summary(make_subset_unit()))
  expect_match(grep("Time to PD start from insertion (mean)", txt, value = TRUE, fixed = TRUE),
               "14\\.0 days$")
  expect_match(grep("Time to first peritonitis episode (mean)", txt, value = TRUE, fixed = TRUE),
               "45\\.0 days$")
  expect_false(any(grepl("n = ", txt, fixed = TRUE)))
  expect_equal(res$catheters$time_to_pd_start_days[["mean"]], 14)
  expect_equal(res$catheters$time_to_first_peritonitis_days[["mean"]],
               as.numeric(d("2025-04-01") - d("2025-02-15")))
})

test_that("summary.pd_unit() prints NA when there are no incident patients", {
  unit <- make_subset_unit()
  unit$patients$new_patient_flag <- FALSE
  txt <- capture.output(summary(unit))
  expect_match(grep("Time to PD start from insertion (mean)", txt, value = TRUE, fixed = TRUE), ": NA$")
  expect_match(grep("Time to first peritonitis episode (mean)", txt, value = TRUE, fixed = TRUE), ": NA$")
})

test_that("summary.pd_unit() timing works on the unit built from the bundled files", {
  a3 <- system.file("extdata", "a3_2025.xlsx", package = "peridial")
  pe <- system.file("extdata", "pe_2025.xlsx", package = "peridial")
  skip_if_not(nzchar(a3) && nzchar(pe))
  unit <- pd_unit(a3, pe, t0 = T0, t1 = T1)
  capture.output(s <- summary(unit))
  tc <- s$catheters
  expect_equal(tc$time_to_pd_start_days[["n"]], unit$n_new)
  expect_equal(tc$time_to_pd_start_days[["mean"]], 15.3, tolerance = 0.01)
  expect_lte(tc$time_to_first_peritonitis_days[["n"]], unit$n_new)
  expect_gt(tc$time_to_first_peritonitis_days[["mean"]], 0)
})

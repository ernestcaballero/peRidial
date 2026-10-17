# Tests for R/ingest_tables.R: flattening the pd_patient object into the unit's patients / catheters / infections tibbles.
#   AAA0001: 2 catheters (one episode each)
#   BBB0002: 1 catheter, no episodes
#   CCC0003: 1 catheter, 3 episodes (the 3rd is a relapse)  -- incident patient

patient_list_3 <- function() make_subset_unit()$patient_list


# patients_to_tibble()

test_that("patients_to_tibble() gives one row per patient, in order, with the reporting window and counts", {
  tbl <- patients_to_tibble(patient_list_3(), T0, T1)
  expect_s3_class(tbl, "tbl_df")
  expect_identical(tbl$patient_id, c("AAA0001", "BBB0002", "CCC0003"))
  expect_true(all(tbl$t0 == T0))
  expect_true(all(tbl$t1 == T1))
  expect_identical(tbl$n_catheters, c(2L, 1L, 1L))
  expect_equal(tbl$n_episodes, c(2, 0, 2))   # CCC0003's relapse is not counted
  expect_identical(tbl$new_patient_flag, c(FALSE, FALSE, TRUE))
})

test_that("patients_to_tibble() reports patient-years that sum to the unit total", {
  pl <- patient_list_3()
  tbl <- patients_to_tibble(pl, T0, T1)
  expect_equal(sum(tbl$patient_years_at_risk), total_patient_years(pl, T0, T1))
  expect_true(all(tbl$patient_years_at_risk > 0))
})

test_that("patients_to_tibble() handles a patient with no catheters", {
  p <- make_patient("NOCATH1", catheters = list())
  tbl <- patients_to_tibble(list(p), T0, T1)
  expect_identical(nrow(tbl), 1L)
  expect_true(is.na(tbl$first_pd_start_date))
  expect_equal(tbl$patient_years_at_risk, 0)
})


# catheters_to_tibble()

test_that("catheters_to_tibble() gives one row per catheter, with dates, procedure and episode counts", {
  tbl <- catheters_to_tibble(patient_list_3(), T0, T1)
  expect_identical(tbl$catheter_id,
                   c("AAA0001_01", "AAA0001_02", "BBB0002_01", "CCC0003_01"))
  expect_identical(tbl$patient_id, c("AAA0001", "AAA0001", "BBB0002", "CCC0003"))
  expect_identical(tbl$procedure_type[1:2], c("laparoscopic", "open surgical"))
  expect_identical(tbl$insertion_date[1], as.Date("2023-05-01"))
  expect_identical(tbl$pd_stop_date[1], as.Date("2025-06-30"))
  expect_true(is.na(tbl$pd_stop_date[2]))
  expect_equal(tbl$n_peritonitis_episodes, c(1, 1, 0, 2))
  expect_identical(tbl$peritonitis_flag, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("catheters_to_tibble() computes exposure days inside the period", {
  tbl <- catheters_to_tibble(patient_list_3(), T0, T1)
  # BBB0002_01 is open the whole year
  expect_equal(tbl$exposure_days_in_period[3], as.numeric(T1 - T0 + 1))
  # AAA0001_01 is clipped to its own stop date
  expect_equal(tbl$exposure_days_in_period[1],
               as.numeric(as.Date("2025-06-30") - T0 + 1))
})

test_that("catheters_to_tibble() censors exposure at the patient's transfer_date (tau)", {
  cath <- make_catheter(pd_start_date = as.Date("2024-06-01"))
  p <- make_patient(catheters = list(cath),
                    transfer_reason = "death", transfer_date = as.Date("2025-03-31"))
  tbl <- catheters_to_tibble(list(p), T0, T1)
  expect_equal(tbl$exposure_days_in_period, as.numeric(as.Date("2025-03-31") - T0 + 1))
})


# infections_to_tibble()

test_that("infections_to_tibble() gives one row per episode, with its owning catheter", {
  tbl <- infections_to_tibble(patient_list_3(), T0, T1)
  expect_identical(nrow(tbl), 5L)
  expect_identical(tbl$catheter_id[tbl$patient_id == "AAA0001"],
                   c("AAA0001_01", "AAA0001_02"))
  expect_true(all(tbl$catheter_id[tbl$patient_id == "CCC0003"] == "CCC0003_01"))
})

test_that("infections_to_tibble() flags relapses as not counting towards the rate", {
  tbl <- infections_to_tibble(patient_list_3(), T0, T1)
  expect_identical(sum(tbl$counts_toward_rate), 4L)
  relapse <- tbl[tbl$patient_id == "CCC0003" & tbl$infection_date == as.Date("2025-08-20"), ]
  expect_false(relapse$counts_toward_rate)
  expect_identical(relapse$episode_type, "relapsing")
})

test_that("infections_to_tibble() collapses organisms into a string and counts them", {
  inf <- make_infection(organism_list = list("E. coli", "Klebsiella"))
  cath <- make_catheter(infections = list(inf))
  tbl <- infections_to_tibble(list(make_patient(catheters = list(cath))), T0, T1)
  expect_identical(tbl$organisms, "E. coli, Klebsiella")
  expect_identical(tbl$n_organisms, 2L)
})

test_that("infections_to_tibble() does not count an episode outside the reporting period", {
  inf <- make_infection(infection_date = as.Date("2024-10-15"))
  cath <- make_catheter(infections = list(inf), t0 = as.Date("2024-01-01"),
                        t1 = as.Date("2024-12-31"))
  p <- make_patient(catheters = list(cath), t0 = as.Date("2024-01-01"),
                    t1 = as.Date("2024-12-31"))
  tbl <- infections_to_tibble(list(p), T0, T1)
  expect_identical(nrow(tbl), 1L)
  expect_false(tbl$counts_toward_rate)
})

test_that("infections_to_tibble() does not count an episode after the patient's tau", {
  inf_in <- make_infection(infection_date = as.Date("2025-11-15"))
  inf_late <- make_infection(infection_date = as.Date("2025-12-15"))
  cath <- make_catheter(infections = list(inf_in, inf_late))   # still open
  p <- make_patient(catheters = list(cath), transfer_reason = "death",
                    transfer_date = as.Date("2025-11-30"))
  tbl <- infections_to_tibble(list(p), T0, T1)
  expect_identical(tbl$counts_toward_rate, c(TRUE, FALSE))
})

test_that("the rate numerator and the patient-years end at the same date", {
  inf_late <- make_infection(infection_date = as.Date("2026-01-20"))
  cath <- make_catheter(pd_stop_date = as.Date("2026-02-15"), infections = list(inf_late))
  p <- make_patient(catheters = list(cath))
  tbl <- infections_to_tibble(list(p), T0, T1)
  expect_equal(sum(tbl$counts_toward_rate), 0)
  expect_equal(total_patient_years(list(p), T0, T1), 365 / 365.25)   # exposure stops at t1
})


# the three tibbles agree with each other

test_that("the flattened tibbles are consistent with one another", {
  pl <- patient_list_3()
  pt <- patients_to_tibble(pl, T0, T1)
  ct <- catheters_to_tibble(pl, T0, T1)
  it <- infections_to_tibble(pl, T0, T1)
  expect_true(all(ct$patient_id %in% pt$patient_id))
  expect_true(all(it$catheter_id %in% ct$catheter_id))
  expect_equal(pt$n_catheters, as.integer(table(factor(ct$patient_id, pt$patient_id))))
  expect_equal(sum(ct$n_peritonitis_episodes), sum(it$counts_toward_rate))
})

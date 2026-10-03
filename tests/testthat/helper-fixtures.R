# Shared test fixtures. testthat sources every helper-*.R file before running the tests,
# so these are available in all test files.
#
# The defaults describe one valid, prevalent patient (ABC1234) with one
# catheter (ABC1234_01) over calendar year 2025.

# Reporting period used throughout the tests
T0 <- as.Date("2025-01-01")
T1 <- as.Date("2025-12-31")

# A catheter inserted and started before T0, still active (no stop date),
# so its patient is prevalent (not incident) in [T0, T1].
make_catheter <- function(patient_id = "ABC1234",
                          catheter_id = "ABC1234_01",
                          insertion_date = as.Date("2024-05-15"),
                          procedure_type = NA_character_,
                          pd_start_date = as.Date("2024-06-01"),
                          pd_stop_date = as.Date(NA),
                          removal_reason = NA_character_,
                          infections = list(),
                          t0 = T0,
                          t1 = T1,
                          ...) {
  new_pd_catheter(
    patient_id = patient_id,
    catheter_id = catheter_id,
    insertion_date = insertion_date,
    procedure_type = procedure_type,
    pd_start_date = pd_start_date,
    pd_stop_date = pd_stop_date,
    removal_reason = removal_reason,
    infections = infections,
    t0 = t0,
    t1 = t1,
    ...
  )
}

make_patient <- function(patient_id = "ABC1234",
                         catheters = list(make_catheter()),
                         t0 = T0,
                         t1 = T1,
                         ...) {
  new_pd_patient(
    patient_id = patient_id,
    catheters = catheters,
    t0 = t0,
    t1 = t1,
    ...
  )
}

# A single peritonitis episode for the default patient. Goes through the
# validating pd_infection() constructor, so overriding a field with something
# invalid errors here rather than in the test body. Defaults describe a plain
# E. coli episode inside the default catheter's PD window; prior_episode is
# passed through so episode_type can be derived.
make_infection <- function(patient_id = "ABC1234",
                           infection_date = as.Date("2025-10-15"),
                           organism_list = list("E. coli"),
                           last_dose_antibiotic = infection_date + 14,
                           ...) {
  pd_infection(
    patient_id = patient_id,
    infection_date = infection_date,
    organism_list = organism_list,
    last_dose_antibiotic = last_dose_antibiotic,
    ...
  )
}

# A unit holding that single prevalent patient. Argument defaults are fixed.
# Only tpyar is computed, from the default cohort over [T0, T1].
make_unit <- function(unit_id = "UNIT1",
                      t0 = T0,
                      t1 = T1,
                      n_new = 0L,
                      n_patients = 1L,
                      tpyar = NULL,
                      rate_benchmark = 0.40,
                      patients = tibble::tibble(patient_id = "ABC1234"),
                      catheters = tibble::tibble(patient_id = "ABC1234",
                                                 catheter_id = "ABC1234_01"),
                      infections = tibble::tibble(),
                      patient_list = list(make_patient())) {
  if (is.null(tpyar)) {
    tpyar <- tryCatch(total_patient_years(patient_list, T0, T1),
                      error = function(e) NA_real_)
  }
  new_pd_unit(
    unit_id = unit_id,
    t0 = t0,
    t1 = t1,
    n_new = n_new,
    n_patients = n_patients,
    tpyar = tpyar,
    rate_benchmark = rate_benchmark,
    patients = patients,
    catheters = catheters,
    infections = infections,
    patient_list = patient_list
  )
}


# Find the benchmark (geom_hline) layer of a ggplot by geom class rather than by position
get_hline_layer <- function(p) {
  hits <- Filter(function(l) inherits(l$geom, "GeomHline"), p$layers)
  stopifnot(length(hits) >= 1)
  hits[[1]]
}

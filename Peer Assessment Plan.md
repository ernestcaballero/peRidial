
# Peer Assessment Plan — peRidial

**Package:** `peridial` — ingests raw peritoneal dialysis (PD) unit data and calculates ISPD-recommended peritonitis rate / peritonitis-free indicators.



**Repository:** https://github.com/ernestcaballero/peRidial


---

## 1. Installation

```r
install.packages("remotes")
remotes::install_github("ernestcaballero/peridial")
library(peridial)
```

The package links to Rcpp, so installing from source compiles C++ code: you need a C++ toolchain (Rtools on Windows, Xcode Command Line Tools on macOS via `xcode-select --install`; nothing extra on Linux). This is the only non-R-package prerequisite.

---

## 2. Architecture: what's OO and what's Rcpp

The package has three layers:

1. **OO layer (S3 classes)** — four nested classes: `pd_unit` → `pd_patient` → `pd_catheter` → `pd_infection`. Every class follows the same pattern: a bare `new_X()` constructor, a `validate_X()` that enforces invariants (including cross-checks against nested objects), a convenience `X()` wrapper that calls both, and `print.X()` / `summary.X()` methods.
2. **Rcpp layer** — one piece of logic is implemented in C++: `classify_episode_types_cpp()` in `src/episode_type.cpp`. It is the naturally-sequential, per-patient loop in the package (matching a patient's episode against its immediately preceding episode (if any) and performs a classification). It's wrapped by the R-facing `classify_episode_types()`, which does the validation in R and hands only the valid rows to C++.
3. **Plain-R layers** — `indicators.R` (pure arithmetic over the S3 objects: exposure days, patient-years) and `ingest.R` (reads raw Excel exports, cleans them, and assembles the object tree).

```
raw Excel (A3 form, PE form)
        │   readxl + standardise_names() + as_date_safe()/as_logical_safe()
        ▼
   pd_unit()                                   [ingest.R — top-level entry point]
        │  builds, per patient:
        ▼
   pd_patient()  ──▶ new_pd_patient()  ──▶ validate_pd_patient()
        │  nests
        ▼
   pd_catheter() ──▶ new_pd_catheter() ──▶ validate_pd_catheter()
        │  nests
        ▼
   pd_infection()──▶ get_episode_type()──▶ new_pd_infection()──▶ validate_pd_infection()
        │                     (vectorised path: classify_episode_types() ──▶ [C++] classify_episode_types_cpp())
        ▼
indicators:  exposure_days() ─▶ catheter_exposure_days() ─▶ total_patient_years()
        ▼
print.*() / summary.pd_patient() / summary.pd_unit()
(print method for all the S3 classes and summary method for pd_unit and pd_patient only)
```

---

## 3. Function inventory (see specific verification lines below)

| Function | Layer | What it does | How to verify |
|---|---|---|---|
| `new_pd_infection()` | OO constructor | Builds a bare `pd_infection` (one peritonitis episode). | `testthat::test_file("tests/testthat/test-pd_infection.R")` |
| `validate_pd_infection()` | OO validator | Enforces field rules: valid organism combos, outcome/outcome_date pairing, `episode_type` is a legal ISPD category. | same file |
| `get_episode_type()` | OO helper (scalar) | Given one episode + its preceding episode, returns `"relapsing"/"recurrent"/"repeat"/NA` — the scalar counterpart to the Rcpp loop. | same file |
| `pd_infection()` | OO convenience constructor | `new_pd_infection()` + `get_episode_type()` + `validate_pd_infection()` in one call. | same file |
| `print.pd_infection()` | OO S3 method | One-episode human-readable snapshot. | run the `@examples` in `?pd_infection` |
| `new_pd_catheter()` | OO constructor | Builds a bare `pd_catheter`, nesting a list of `pd_infection` objects. | `testthat::test_file("tests/testthat/test-pd_catheter.R")` |
| `validate_pd_catheter()` | OO validator | Date consistency (`insertion ≤ pd_start ≤ pd_stop`), exposure-day bounds, nested infections checked against the catheter's own window. | same file |
| `count_episodes_in_period()` | OO internal helper | Counts a catheter's countable (non-relapse) episodes inside `[t0,t1]`, open- or closed-window. | same file |
| `pd_catheter()` | OO convenience constructor | `new_pd_catheter()` + `validate_pd_catheter()`. | same file |
| `print.pd_catheter()` | OO S3 method | Catheter snapshot including countable + relapsing episodes. | `?pd_catheter` examples |
| `new_pd_patient()` | OO constructor | Builds a bare `pd_patient`, nesting `pd_catheter` objects; derives `new_patient_flag`. | `testthat::test_file("tests/testthat/test-pd_patient.R")` |
| `validate_pd_patient()` | OO validator | Cross-object rules: nested catheter ids/windows/overlaps vs. the patient's own window and transfer date; derived counts match supplied data. | same file |
| `is_incident_patient()` | OO internal helper | TRUE/FALSE/NA — was the earliest `pd_start_date` inside `[t0,t1]`? | same file |
| `count_patient_episodes()` | OO internal helper | Sums countable episodes across a patient's catheters. | exercised indirectly by `test-pd_patient.R`; no isolated test yet |
| `pd_patient()` | OO convenience constructor | `new_pd_patient()` + `validate_pd_patient()`. | `test-pd_patient.R` |
| `print.pd_patient()` | OO S3 method | One-patient snapshot. | `?pd_patient` examples |
| `summary.pd_patient()` | OO S3 method | Fuller patient summary (censoring, counts). | `?pd_patient` examples |
| `new_pd_unit()` | OO constructor | Builds a bare `pd_unit`, nesting `pd_patient` objects plus unit-level counts. | `testthat::test_file("tests/testthat/test-pd_unit.R")` |
| `validate_pd_unit()` | OO validator | Cross-checks unit tibbles/counts against `patient_list` (duplicate ids, orphaned catheter/infection ids, count consistency). | same file |
| `pd_unit()` | Ingest pipeline | Reads the raw A3 (demographics/catheters) and PE (infections) Excel exports, cleans them, assembles the full nested object tree. | See 4.0 — uses the bundled fixtures in `inst/extdata/` |
| `print.pd_unit()` | OO S3 method | One-unit snapshot. | `print(unit)` on the object from 4.0 |
| `summary.pd_unit()` | OO S3 method | Full unit report: peritonitis rate, peritonitis-free %, episode-type breakdown, outcomes, cohort/demographics, catheter & infection tables. Backed by internal `summarise_*()` helpers. | `summary(unit)` on the object from 4.0 |
| `classify_episode_types()` | Rcpp wrapper (R) | Vectorised `get_episode_type()`: validates each row via the `pd_infection`, dispatches valid rows to C++. | `testthat::test_file("tests/testthat/test-classify_episode_types.R")` |
| `classify_episode_types_cpp()` | **Rcpp / C++** (`src/episode_type.cpp`) | Pairs each valid episode with its patient's immediately preceding one, applies the 28-day/organism-match rule. | exercised through the R wrapper above; direct call: `peridial:::classify_episode_types_cpp(...)` |
| `exposure_days()` | Indicator (plain R) | `max(0, max_date − min_date + 1)` — shared day-counting primitive. | manual: `exposure_days(as.Date("2025-01-01"), as.Date("2025-01-10"))` → `10` |
| `catheter_exposure_days()` | Indicator (plain R) | One catheter's days-at-risk, clipped to `[t0,t1]`, censored at the patient's `tau`. | manual, against a `pd_catheter` object |
| `total_patient_years()` | Indicator (plain R) | Sums `catheter_exposure_days()` across all patients/catheters ÷ 365.25 → unit PYAR denominator. | called inside `test-pd_unit.R` |

**Internal (non-exported) helpers**

- `ingest.R`: `standardise_names`, `require_cols`, `ensure_cols`, `as_date_safe`, `as_logical_safe`, `new_issue_log`, `report_issues`, `create_catheter_id`, `find_transplant_date`, `patient_demo_value`, `patient_dob_value`, `derive_patient_tau`, `on_pd_in_period`, `patients_to_tibble`, `catheters_to_tibble`, `infections_to_tibble` —covered only via `pd_unit()`'s worked example (4.0). 

- `pd_unit.R`: `require_unit_cols`, `has_col`, `summarise_rate`, `summarise_pf`, `summarise_episode_types`, `summarise_outcomes`, `summarise_cohort`, `summarise_median_age`, `summarise_demographics`, `days_quartiles`, `summarise_catheters`, `summarise_infections`

---

## 4. Verify

### 4.1 Install + load

```r
install.packages("remotes")
remotes::install_github("ernestcaballero/peridial")
library(peridial)
```

### 4.2 Run the automated suite and RMD check:
```r
devtools::test()      # all files in tests/testthat should pass
testthat::test_dir(system.file("tests", "testthat", package = "peridial"))

devtools::check()     # expect 0 errors, 0 warnings
```


### 4.3 How to check each function

Run the setup once, then work through the checks. 
Every check shows the call
and what you should see. Anything marked *should error* is a deliberate failure case.

```r
library(peridial)
t0 <- as.Date("2025-01-01"); t1 <- as.Date("2025-12-31")

unit <- pd_unit(
  unit_data_path      = system.file("extdata", "a3_2025.xlsx", package = "peridial"),
  infection_data_path = system.file("extdata", "pe_2025.xlsx", package = "peridial"),
  t0 = t0, t1 = t1, unit_id = "Wellington PD Unit"
)
```

### 4.3.1 `pd_unit()`, `print()`, `summary()`

| Check | Call | Expected |
|---|---|---|
| Unit builds | `class(unit)` | `"pd_unit"` |
| Print method | `print(unit)` | Header with unit name, period, patient / incident counts, catheters, episodes, patient-years |
| Summary method | `summary(unit)` | Rate, peritonitis-free %, episode types, outcomes, demographics, catheter and infection summaries |
| Cohort size agrees | `unit$n_patients == nrow(unit$patients)` and `== length(unit$patient_list)` | `TRUE`, `TRUE` |
| Incident subset | `unit$n_new <= unit$n_patients` | `TRUE` |
| Bad counts rejected | `peridial:::validate_pd_unit(new_pd_unit(unit_id = "X", t0 = t0, t1 = t1, n_new = 5L, n_patients = 2L, tpyar = 1))` | *Should error:* "n_new cannot exceed n_patients." |


### 4.3.2 Nested objects (`pd_patient`, `pd_catheter`, `pd_infection`)

```r
pat  <- unit$patient_list[[1]]
cath <- pat$catheters[[1]]
```

| Check | Call | Expected |
|---|---|---|
| Classes | `class(pat)`, `class(cath)` | `"pd_patient"`, `"pd_catheter"` |
| Print / summary | `print(pat)`, `summary(pat)`, `print(cath)` | Readable multi-line output, no errors |
| Child belongs to parent | `cath$patient_id == pat$patient_id` | `TRUE` |
| Catheter count | `pat$n_catheters == length(pat$catheters)` | `TRUE` |
| Episode roll-up | `pat$n_episodes == sum(vapply(pat$catheters, function(c) c$n_peritonitis_episodes, numeric(1)))` | `TRUE` |
| Flag matches count | `cath$peritonitis_flag == (cath$n_peritonitis_episodes > 0)` | `TRUE` |
| Missing IDs rejected | `peridial:::validate_pd_catheter(new_pd_catheter(patient_id = NA_character_, catheter_id = NA_character_, insertion_date = as.Date("2025-01-25"), pd_start_date = as.Date(NA), pd_stop_date = as.Date(NA), t0 = t0, t1 = t1))` | *Should error* (patient_id / catheter_id required) |
| Bare `NA` date rejected | `new_pd_catheter(patient_id = "ABC0110", catheter_id = "ABC0110_01", insertion_date = as.Date("2025-01-25"), pd_start_date = NA, t0 = t0, t1 = t1)` | *Should error:* pd_start_date must be a Date |
| Outcome needs a date | `peridial:::validate_pd_infection(new_pd_infection(patient_id = "ABC0110", infection_date = as.Date("2025-03-01"), organism_list = list("Staphylococcus aureus"), episode_type = NA_character_, last_dose_antibiotic = as.Date("2025-03-15"), outcome = "catheter removed", outcome_date = as.Date(NA)))` | *Should error:* "Missing date of catheter removal. Must be supplied." |


### 4.3.3 Exposure functions

| Call | Expected |
|---|---|
| `exposure_days(as.Date("2025-01-01"), as.Date("2025-01-10"))` | `10` (both endpoints counted) |
| `exposure_days(as.Date("2025-02-01"), as.Date("2025-01-01"))` | `0` (never negative) |
| `catheter_exposure_days(list(pd_start_date = as.Date("2025-02-10"), pd_stop_date = as.Date(NA)), t0, t1)` | `325` (open-ended catheter closes at `t1`) |
| Same catheter with `tau = as.Date("2025-06-30")` | `141` (censored at patient's leaving date) |
| `total_patient_years(unit$patient_list, t0, t1)` | Equals `unit$tpyar` |


### 4.3.4 Peritonitis episode classification

```r
prior <- new_pd_infection(
  patient_id = "ABC0110", infection_date = as.Date("2025-03-01"),
  organism_list = list("Staphylococcus aureus"), episode_type = NA_character_,
  last_dose_antibiotic = as.Date("2025-03-15"),
  outcome = NA_character_, outcome_date = as.Date(NA)
)
```

| Call | Expected |
|---|---|
| `get_episode_type(as.Date("2025-03-01"), list("Staphylococcus aureus"), NULL)` | `NA` (first episode) |
| `get_episode_type(as.Date("2025-03-25"), list("Staphylococcus aureus"), prior)` | `"relapsing"` (same organism, within 4 weeks) |
| `get_episode_type(as.Date("2025-03-25"), list("Escherichia coli"), prior)` | `"recurrent"` (different organism, within 4 weeks) |
| `get_episode_type(as.Date("2025-06-15"), list("Staphylococcus aureus"), prior)` | `"repeat"` (same organism, after 4 weeks) |
| `classify_episode_types(rep("ABC0110", 3), as.Date(c("2025-03-01","2025-03-25","2025-06-15")), as.Date(c("2025-03-15","2025-04-08","2025-06-29")), list(list("Staphylococcus aureus"), list("Staphylococcus aureus"), list("Staphylococcus aureus")))` | `NA`, `"relapsing"`, `"repeat"` (matches the scalar function row by row) |


# 4. spot-check the Rcpp path directly
classify_episode_types(
  patient_id = c("patient01", "patient01"),
  infection_date = as.Date(c("2025-04-01", "2025-05-12")),
  last_dose_antibiotic = as.Date(c("2025-04-15", "2025-06-01")),
  organism_list = list(list("E. coli"), list("E. coli"))
)
#> [1] NA "relapsing"


# 5. Known limitations and assumptions

- **Exposure days on the catheter.** `total_exposure_days` is not derived
  inside `pd_catheter`, because it needs the patient's leaving date (`tau`),
  which the catheter doesn't know. It is supplied from higher up, and defaults
  to `NA` when not supplied. `pd_unit()` fills it for every
  catheter in the bundled data.
- **Limited public interface.** The `validate_*()` functions and the summary
  helpers are internal. The examples above use `peridial:::` to reach the
  validators. There is no exported function for the individual ISPD
  indicators beyond `summary()`.
- **Nothing beyond the 2025 layout.** `pd_unit()` expects the column names and
  sheet layout of the two bundled Excel files.
- Relapsing episodes are excluded from `n_peritonitis_episodes` and
  `n_episodes`; recurrent and repeat episodes are counted. Episodes are
  counted only when they fall inside both the catheter's active PD window and
  the reporting period `[t0, t1]`.
- Episode type is judged against the same patient's immediately preceding
  valid episode only. A patient's earlier history is not consulted (eg. outside reporting period).
- An `NA` stop date means the catheter is still in use, and an `NA` transfer
  date means the patient never left PD. Both close at `t1`.
- Dates must be genuine `Date` values. A bare `NA` is rejected for
  `pd_start_date` and `pd_stop_date`, and normalised to `as.Date(NA)` for
  `date_of_birth` and `transfer_date`.

**Not yet tested or not covered**

- Behaviour with very large units (performance is untested).
- Patients with several catheters overlapping in time. Not tested whether
  exposure could be counted twice.
- Free-text category fields (ethnicity, diabetes status, smoking status,
  primary kidney disease) are not checked against an allowed list.
```

Data:
The package will be developed and distributed using synthetic data only. No real patient records
will be accessed, stored, or shipped, so no ethics approval will be required. Patient identifiers in the
sample dataset will be synthetic.
---

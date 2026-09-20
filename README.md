# peRidial

A standardised R framework for ingesting, validating, and summarising clinical peritoneal dialysis (PD) unit data. Nested S3 classes represent PD patients, catheters, and peritonitis episodes, supporting reproducible calculation of ISPD-recommended peritonitis rate and peritonitis-free percentage indicators from longitudinal catheter and infection records.

## Installation

Install the development version of peRidial from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("ernestcaballero/peridial")
```

> **Note:** peRidial contains compiled C++ code (via Rcpp), installing it from GitHub compiles that code on your machine rather than downloading a pre-built binary. This requires a C++ compiler: on Windows, install Rtools first; on macOS, run xcode-select --install to get the Xcode Command Line Tools. If with Linux systems, so no extra step is needed there.

## Usage: OO R function using Rcpp

`classify_episode_types()` is a vectorised counterpart to `get_episode_type()` for classifying many peritonitis episodes at once. Each row is first built and validated exactly as `pd_infection()` would check it — via `new_pd_infection()` and `validate_pd_infection()` — then valid rows are classified as `"relapsing"`, `"recurrent"`, or `"repeat"` (per ISPD 2022 definitions) against the same patient's immediately preceding valid episode. The classification loop itself runs in C++ (`classify_episode_types_cpp()`, in `src/`) and is wrapped for use in R.

```r
library(peridial)  # use all lowercase

classify_episode_types(
  patient_id = c("patient01", "patient01", "patient02", "patient02"),
  infection_date = as.Date(c("2025-04-01", "2025-05-12", "2025-03-31", "2025-07-13")),
  last_dose_antibiotic = as.Date(c("2025-04-15", "2025-06-01", "2025-04-14", "2025-07-27")),
  organism_list = list(list("E. coli"), list("E. coli"), list("E. coli"), list("E. coli"))
)
#> [1] NA          "relapsing" NA          "repeat"
```

## Tests

Each core S3 class (`pd_infection`, `pd_catheter`, `pd_patient`, `pd_unit`) and the vectorised `classify_episode_types()` function has its own test file, and each can be run in isolation:

```r
testthat::test_file("tests/testthat/test-classify_episode_types.R")
testthat::test_file("tests/testthat/test-pd_catheter.R")
testthat::test_file("tests/testthat/test-pd_patient.R")
```

`devtools::test()` runs the whole suite at once. For now, only unit tests for function with Rcpp implementation `classify_episode_type()` and R objects `pd_catheter` and `pd_patient` will be shown here. The unit tests under `classify_episode_type()` is similar to `pd_infection` unit tests in terms of episode type derivation checks.

Across the three files below: 64 `test_that()` blocks, 74 individual expectations, all passing, 0 failures, 0 warnings, 0 skips.

### `test-classify_episode_types.R`

13 `test_that()` blocks (16 expectations — several blocks check more than one thing). What is included:

- the function's output contract: return type and length — `expect_type()`, `expect_length()`
- the ISPD classification logic itself: relapsing, recurrent, repeat, and the no-category case — `expect_equal()`, `expect_true()`
- organism matching that's case- and order-insensitive — `expect_equal()`
- episodes are never compared across different patients — `expect_equal()`
- rows that fail internal `pd_infection` validation (e.g. a `"catheter removed"` outcome with no `outcome_date`, or a missing `last_dose_antibiotic`) are silently excluded as comparison points rather than misclassified — `expect_equal()`
- malformed input at the call level — wrong argument type, or mismatched vector lengths — raises an informative error — `expect_error()`

```
> testthat::test_file("tests/testthat/test-classify_episode_types.R")
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 16 ]
```

### `test-pd_catheter.R`

21 `test_that()` blocks (23 expectations). What is included:

- `new_pd_catheter()` produces a valid `pd_catheter` S3 object — `expect_s3_class()`
- required fields (`patient_id`/`catheter_id`, `insertion_date`, `pd_start_date`) must be present — `expect_error()`
- dates must be internally consistent: `insertion_date` on or before `pd_start_date`, `pd_start_date` on or before `pd_stop_date`, and `pd_stop_date` required whenever `removal_reason` is given — `expect_error()`
- `total_exposure_days` can't be negative, and can't exceed the catheter's active span whether that span is closed (`pd_stop_date`) or still open (falls back to `t1`) — `expect_error()`
- the `count_episodes_in_period()` helper's date-windowing logic, tested in isolation: uses `pd_stop_date` as the upper bound when present, falls back to `t1` for a still-active catheter, and raises the lower bound to `t0` when `pd_start_date` predates the reporting period — `expect_identical()`
- the nested `infections` list: each element must be a `pd_infection` object with a matching `patient_id`, a present `infection_date`, and a date falling inside the catheter's own active window — `expect_error()`
- the catheter's derived summary fields, `n_peritonitis_episodes` and `peritonitis_flag`, are cross-checked against the infections actually supplied rather than trusted as given — `expect_error()`
- a fully consistent object passes through `validate_pd_catheter()` unchanged — `expect_identical()`

```
> testthat::test_file("tests/testthat/test-pd_catheter.R")
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 23 ]
```

### `test-pd_patient.R`

30 `test_that()` blocks (35 expectations). What is included:

- basic construction and field checks: a valid object is returned, and errors are raised for a missing `patient_id` or `t0` after `t1` — `expect_s3_class()`, `expect_error()`
- the `transfer_date`/`transfer_reason` pairing rule: each must be supplied together or not at all — `expect_error()`
- a bare `NA` for `transfer_date` is coerced into a proper `Date` `NA` — `expect_true()`, `expect_s3_class()`, `expect_silent()`
- the `is_incident_patient()` helper, tested in isolation: returns `TRUE` when the earliest `pd_start_date` across a patient's catheters falls inside `[t0, t1]`, `FALSE` when it predates the window, and `NA` when there's no catheter, no usable `pd_start_date`, or only non-`pd_catheter` elements — `expect_true()`/`expect_false()`
- `new_pd_patient()` derives `new_patient_flag` from that helper by default — `expect_true()`
- `validate_pd_patient()`'s cross-object consistency rules: each nested catheter's `patient_id` must match the patient's; each catheter must be a genuine `pd_catheter` object with `pd_start_date`/`pd_stop_date` present; catheter dates can't fall outside the patient's `transfer_date`; a nested catheter's reporting window must match the patient's own `[t0, t1]`; catheter intervals within a patient must not overlap; and the derived `n_catheters`/`n_episodes` counts must match what's actually supplied — `expect_error()`

```
> testthat::test_file("tests/testthat/test-pd_patient.R")
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 35 ]
```

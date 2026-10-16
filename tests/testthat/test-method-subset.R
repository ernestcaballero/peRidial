# Tests for R/method-subset.R for helpers flatten_pd_tables() and subset_table()
#
# This file covers the helpers directly and what holds across the methods.
#
# make_subset_unit() (helper-fixtures.R) has three patients and six flat rows:
#   AAA0001: 2 catheters, one episode each (NA, then repeat)
#   BBB0002: 1 catheter, no episodes
#   CCC0003: 1 catheter, 3 episodes (NA, repeat, relapsing)
# 5 episodes, 4 of which count towards the rate.

d <- function(x) as.Date(x)

flat_patients <- function() {
  tibble::tibble(patient_id = c("P1", "P2", "P3"), age = c(50, 60, 70))
}
flat_catheters <- function() {
  tibble::tibble(patient_id = c("P1", "P1", "P2"),
                 catheter_id = c("P1_01", "P1_02", "P2_01"),
                 site = c("a", "b", "c"))
}
flat_infections <- function() {
  tibble::tibble(patient_id = c("P1", "P2", "P2"),
                 catheter_id = c("P1_01", "P2_01", "P2_01"),
                 infection_date = d(c("2025-03-01", "2025-04-01", "2025-05-01")))
}
sub_df <- function() {
  data.frame(a = 1:5, b = letters[1:5], flag = c(TRUE, FALSE, NA, TRUE, FALSE),
             stringsAsFactors = FALSE)
}
no_sel <- function() rlang::quo()    # a missing quosure: all rows / all columns


# flatten_pd_tables()

test_that("flatten_pd_tables() joins down the hierarchy, one row per episode", {
  out <- flatten_pd_tables(flat_patients(), flat_catheters(), flat_infections())
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out),
                   c("patient_id", "age", "catheter_id", "site", "infection_date"))   # each key once
  expect_identical(out$catheter_id, c("P1_01", "P1_02", "P2_01", "P2_01", NA))
  expect_identical(out$infection_date,
                   d(c("2025-03-01", NA, "2025-04-01", "2025-05-01", NA)))

  # a parent with no children keeps a row with NA below it; a parent's values repeat on each child row
  p3 <- out[out$patient_id == "P3", ]
  expect_equal(nrow(p3), 1L)
  expect_true(is.na(p3$catheter_id) && is.na(p3$infection_date))
  p2 <- out[out$patient_id == "P2", ]
  expect_equal(nrow(p2), 2L)
  expect_true(all(p2$age == 60 & p2$site == "c"))
})

test_that("flatten_pd_tables() suffixes a non-key column shared by two tables, on the lower one", {
  pat <- flat_patients(); pat$note <- "patient"
  cath <- flat_catheters(); cath$note <- "catheter"
  inf <- flat_infections(); inf$note <- "infection"
  out <- flatten_pd_tables(pat, cath, inf)
  expect_identical(names(out),
                   c("patient_id", "age", "note", "catheter_id", "site", "note_catheter",
                     "infection_date", "note_infection"))
  expect_true(all(out$note == "patient"))
})

test_that("flatten_pd_tables() says which table has a problem", {
  # a lower table missing its key column
  expect_error(flatten_pd_tables(flat_patients(), flat_catheters()[, c("catheter_id", "site")]),
               "The catheters table is missing required column\\(s\\): `patient_id`")
  expect_error(flatten_pd_tables(flat_patients(), flat_catheters(),
                                 flat_infections()[, c("patient_id", "infection_date")]),
               "The infections table is missing required column\\(s\\): `catheter_id`")
  # the table above has nothing to join on
  expect_error(flatten_pd_tables(flat_patients(), NULL, flat_infections()),
               "Cannot join `infections`: the table above it has no catheter_id column")
  expect_error(flatten_pd_tables(flat_patients()[, "age", drop = FALSE], flat_catheters()),
               "Cannot join `catheters`: the table above it has no patient_id column")
})


# subset_table()

test_that("subset_table() keeps the rows where the condition is TRUE", {
  df <- sub_df()
  before <- df
  expect_equal(subset_table(df, no_sel(), no_sel()), tibble::as_tibble(df))   # nothing asked: everything, as a tibble
  expect_identical(subset_table(df, rlang::quo(a > 2), no_sel())$a, 3:5)
  expect_identical(subset_table(df, rlang::quo(flag), no_sel())$a, c(1L, 4L))   # an NA condition counts as FALSE
  expect_identical(subset_table(df, rlang::quo(!flag), no_sel())$a, c(2L, 5L))
  expect_equal(nrow(subset_table(df, rlang::quo(TRUE), no_sel())), 5L)          # a single TRUE / FALSE is recycled
  none <- subset_table(df, rlang::quo(FALSE), no_sel())
  expect_equal(nrow(none), 0L)
  expect_identical(names(none), c("a", "b", "flag"))
  expect_identical(df, before)                                                 # the input is unchanged
})

test_that("subset_table() reports a bad condition or select argument", {
  expect_error(subset_table(sub_df(), rlang::quo(a), no_sel()),
               "The `subset` condition `a` must be logical, not integer")
  expect_error(subset_table(sub_df(), rlang::quo(b), no_sel()), "must be logical, not character")
  expect_error(subset_table(sub_df(), rlang::quo(c(TRUE, FALSE)), no_sel()),
               "returned 2 value\\(s\\) for 5 row\\(s\\)")
  expect_error(subset_table(sub_df(), rlang::quo(no_such > 1), no_sel()),
               "Could not evaluate the `subset` condition `no_such > 1`")
})


# the subset() methods

test_that("subset(unit) is one row per episode, laid out patient, catheter, episode, and returns stored values", {
  x <- make_subset_unit()
  before <- x
  y <- subset(x)

  # one row per episode, plus a row for each catheter that has none
  expect_equal(nrow(y), nrow(x$infections) + sum(!x$catheters$catheter_id %in% x$infections$catheter_id))
  expect_equal(nrow(y), 6L)
  expect_identical(
    names(y),
    c(names(x$patients),
      setdiff(names(x$catheters), "patient_id"),
      setdiff(names(x$infections), c("patient_id", "catheter_id"))))
  expect_false(any(c("tpyar", "n_patients", "n_new") %in% names(y)))   # the unit's headline numbers are not in the table

  # values are the stored ones, not recalculated for the rows left: CCC0003 has 3 episode rows, 2 countable
  expect_true(all(subset(x, patient_id == "CCC0003")$n_episodes == 2))
  expect_equal(subset(x, episode_type == "relapsing")$n_episodes, 2)

  # the rows that count towards the rate are the rate numerator
  counted <- subset(x, counts_toward_rate)
  expect_equal(nrow(counted), sum(x$infections$counts_toward_rate))
  capture.output(s <- summary(x))
  expect_equal(nrow(counted), s$rate_num)
  expect_identical(subset(x, !counts_toward_rate)$episode_type, "relapsing")

  expect_identical(x, before)                                           # the unit is not modified
})

test_that("subset(unit) takes the same conditions and select as base subset(), plus tidy evaluation", {
  x <- make_subset_unit()

  # the caller's variables, and a column beats a variable of the same name
  cutoff <- d("2025-06-01")
  expect_equal(nrow(subset(x, infection_date > cutoff)), 3L)
  expect_equal(nrow(subset(x, .data$infection_date > .env$cutoff)), 3L)
  gender <- "Male"
  expect_equal(nrow(subset(x, gender == "Female")), 5L)
  rm(gender)

  # inside a wrapper function, and with !!
  by_gender <- function(unit, g) subset(unit, gender == {{ g }})
  expect_equal(nrow(by_gender(x, "Female")), 5L)
  want <- "Male"
  expect_equal(nrow(subset(x, gender == !!want)), 1L)

  # select by negation and position; arguments by name in any order
  all_names <- names(subset(x))
  expect_identical(names(subset(x, select = -c(t0, t1))), setdiff(all_names, c("t0", "t1")))
  expect_identical(names(subset(x, select = 1:2)), all_names[1:2])
  y <- subset(x, select = c(patient_id, gender), subset = gender == "Male")
  expect_identical(y$patient_id, "BBB0002")
  expect_identical(names(y), c("patient_id", "gender"))
})

test_that("subset(patient) and subset(catheter) give the same rows as subset(unit), and name their class in errors", {
  x <- make_subset_unit()
  cols <- quote(c(catheter_id, infection_date, episode_type, counts_toward_rate))

  for (p in x$patient_list) {
    # transfer_detail is only held on the unit's tibble, so the patient method leaves it out
    expect_equal(subset(p), subset(x, patient_id == p$patient_id, select = -transfer_detail),
                 info = p$patient_id)
    for (cath in p$catheters) {
      from_unit <- eval(bquote(subset(x, catheter_id == .(cath$catheter_id), select = .(cols))))
      from_cath <- eval(bquote(subset(cath, select = .(cols))))
      expect_equal(from_cath, from_unit, info = cath$catheter_id)
    }
  }

  expect_error(subset(x, extra = 1), "A pd_unit takes only a condition and `select`")
  expect_error(subset(x$patient_list[[1]], extra = 1), "A pd_patient takes only a condition and `select`")
  expect_error(subset(x$patient_list[[1]]$catheters[[1]], extra = 1),
               "A pd_catheter takes only a condition and `select`")
})


# edge case

test_that("edge case: tables with no columns or no rows are handled", {
  # a lower table with no columns is skipped; one with columns but no rows keeps every parent row
  expect_equal(flatten_pd_tables(flat_patients(), tibble::tibble()), flat_patients())
  out <- flatten_pd_tables(flat_patients(), flat_catheters()[0, ])
  expect_equal(nrow(out), 3L)
  expect_true(all(is.na(out$catheter_id)))

  # a condition on a table with no rows gives no rows, with the columns kept
  empty <- subset_table(sub_df()[0, ], rlang::quo(a > 2), no_sel())
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), c("a", "b", "flag"))
})

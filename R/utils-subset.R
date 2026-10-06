
# SUBSET METHOD #

#' Join the patients, catheters and infections tables into one flat table
#'
#' Shared by \code{subset.pd_unit()}, \code{subset.pd_patient()} and
#' \code{subset.pd_catheter()}. The tables are left-joined down the hierarchy,
#' patients to catheters on \code{patient_id} and then to infections on
#' \code{patient_id} and \code{catheter_id}.
#'
#' @param patients,catheters,infections Data frames, or \code{NULL} to leave a
#'   level out.
#'
#' @return A tibble, one row per episode (or per catheter, or per patient,
#'   when there is nothing below it).
#' @noRd
#'
flatten_pd_tables <- function(patients = NULL, catheters = NULL, infections = NULL) {
  join_child <- function(parent, child, by, suffix, what) {
    child <- tibble::as_tibble(child)
    if (ncol(child) == 0) {
      return(parent)
    }
    require_unit_cols(child, by, what)
    absent <- setdiff(by, names(parent))
    if (length(absent) > 0) {
      stop("Cannot join `", what, "`: the table above it has no ",
           paste(absent, collapse = ", "), " column.", call. = FALSE)
    }
    dplyr::left_join(parent, child, by = by, suffix = suffix)
  }

  out <- tibble::as_tibble(if (is.null(patients)) catheters else patients)
  if (!is.null(patients) && !is.null(catheters)) {
    out <- join_child(out, catheters, "patient_id", c("", "_catheter"),
                      "catheters")
  }
  if (!is.null(infections)) {
    out <- join_child(out, infections, c("patient_id", "catheter_id"),
                      c("", "_infection"), "infections")
  }
  out
}


#' Apply a base-style subset and select to a data frame
#'
#' Return subsets of data frames which meet conditions. The condition is evaluated
#' with the columns in scope (and the caller's variables), \code{NA} counts as \code{FALSE}, and
#' \code{select} is evaluated with each column name standing for its position,
#' so \code{c(a, b)}, \code{a:b} and \code{-a} all work.
#'
#' @param df A data frame.
#' @param q_subset,q_select Quosures from \code{rlang::enquo()}. A missing
#'   quosure means "all rows" or "all columns".
#'
#' @return \code{df} with the rows and columns selected, as a tibble.
#' @noRd
#'
subset_table <- function(df, q_subset, q_select) {
  n <- nrow(df)

  rows <- if (rlang::quo_is_missing(q_subset)) {
    rep(TRUE, n)
  } else {
    r <- tryCatch(
      rlang::eval_tidy(q_subset, data = df),
      error = function(e) {
        stop("Could not evaluate the `subset` condition `",
             rlang::quo_text(q_subset), "`: ", conditionMessage(e),
             call. = FALSE)
      }
    )
    if (!is.logical(r)) {
      stop("The `subset` condition `", rlang::quo_text(q_subset),
           "` must be logical, not ", class(r)[1], ".", call. = FALSE)
    }
    if (length(r) == 1L) {
      r <- rep(r, n)
    }
    if (length(r) != n) {
      stop("The `subset` condition `", rlang::quo_text(q_subset),
           "` returned ", length(r), " value(s) for ", n, " row(s).",
           call. = FALSE)
    }
    r & !is.na(r)
  }

  cols <- if (rlang::quo_is_missing(q_select)) {
    rep(TRUE, ncol(df))
  } else {
    positions <- as.list(seq_along(df))
    names(positions) <- names(df)
    tryCatch(
      rlang::eval_tidy(q_select, data = positions),
      error = function(e) {
        stop("Could not evaluate the `select` argument `",
             rlang::quo_text(q_select), "`: ", conditionMessage(e),
             call. = FALSE)
      }
    )
  }

  tibble::as_tibble(df)[rows, cols, drop = FALSE]
}


#' Subset a pd_unit object
#'
#' Returns the rows (and, with \code{select}, the columns) of a unit's data
#' that meet a condition, as a plain tibble. Peritonitis indicators are not
#' recalculated and the unit is not modified.
#'
#' The unit's \code{patients}, \code{catheters} and \code{infections} tibbles
#' are first joined into one flat table: one row per peritonitis episode,
#' carrying its catheter's and its patient's columns. A patient with no catheter
#' episodes still has a row. A patient with several catheters or episodes has several rows.
#'
#' @param x A \code{pd_unit} object.
#' @param subset A logical condition on the columns of the flat table (for
#'   example \code{gender == "Female"} or \code{episode_type == "repeat"}).
#'   Omit to keep every row.
#' @param select Which columns to return, written as in
#'   \code{\link[base]{subset}()}: names (\code{c(patient_id, gender)}),
#'   ranges (\code{patient_id:gender}), or negation (\code{-t0}). Omit to
#'   keep every column.
#' @param ... Ignored.
#'
#' @return A tibble.
#' @seealso \code{\link{subset.pd_patient}()} and
#'   \code{\link{subset.pd_catheter}()} for the same on one patient or catheter.
#' @export
#'
#' @examples
#' unit <- pd_unit(
#'   unit_data_path = system.file("extdata", "a3_2025.xlsx", package = "peridial"),
#'   infection_data_path = system.file("extdata", "pe_2025.xlsx", package = "peridial"),
#'   t0 = as.Date("2025-01-01"), t1 = as.Date("2025-12-31"),
#'   unit_id = "Wellington PD Unit"
#' )
#'
#' # every row for female patients
#' subset(unit, gender == "Female")
#'
#' # surgical catheters, with just a few columns
#' subset(unit, procedure_type == "Surgical",
#'        select = c(patient_id, gender, catheter_id, procedure_type, infection_date))
#'
#' # rows that have an episode
#' subset(unit, !is.na(infection_date), select = patient_id:gender)
#'
subset.pd_unit <- function(x, subset, select, ...) {
  if (...length() > 0L) {
    stop("Unused argument(s) passed to subset(). A pd_unit takes only a ",
         "condition and `select`.", call. = FALSE)
  }

  flat <- flatten_pd_tables(x$patients, x$catheters, x$infections)
  subset_table(flat, rlang::enquo(subset), rlang::enquo(select))
}


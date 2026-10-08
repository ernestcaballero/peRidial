
# ingest_read contains helper functions for reading raw files,
# standardising names, map columns, etc


#' Standardise raw column names to snake_case
#' @noRd
standardise_names <- function(df) {
  nms <- trimws(names(df))
  nms <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nms)   # camelCase -> camel_Case
  nms <- gsub("[^A-Za-z0-9]+", "_", nms)             # spaces/punctuation -> _
  nms <- tolower(nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  names(df) <- nms
  df
}



#' Test a standardised column name against a keyword, returns TRUE or FALSE
#' @noRd
matches_keywords <- function(name, rule) {
  if (name %in% rule$exact) {
    return(TRUE)
  }
  if (is.null(rule$all) && is.null(rule$any)) {
    return(FALSE)
  }
  has <- function(kw) grepl(kw, name, fixed = TRUE)
  all_ok <- all(vapply(rule$all, has, logical(1)))
  any_ok <- is.null(rule$any) || any(vapply(rule$any, has, logical(1)))
  all_ok && any_ok
}




#' Rename standardised column names to the names the ingest expects, after matching with keywords
#' @noRd
map_columns <- function(df, spec, what, log = NULL) {
  nms <- names(df)
  new_nms <- nms
  claimed <- nms %in% names(spec)

  for (key in setdiff(names(spec), nms)) {
    is_hit <- vapply(nms, matches_keywords, logical(1), rule = spec[[key]],
                     USE.NAMES = FALSE)
    hits <- which(is_hit & !claimed)
    if (length(hits) == 0) {
      next
    }
    if (length(hits) > 1 && !is.null(log)) {
      log$add("The ", what, " file has ", length(hits), " columns that could be `",
              key, "` (", paste(nms[hits], collapse = ", "), "); used `",
              nms[hits[1]], "`. Rename the columns in the source file if that is wrong.")
    }
    new_nms[hits[1]] <- key
    claimed[hits[1]] <- TRUE
  }

  names(df) <- new_nms
  df
}



#' Error if a data frame is missing columns that are required
#'
#' Used on the raw files (\code{kind = "file"}, after name standardisation) and
#' on the unit's own tibbles when validating (\code{kind = "table"}). A missing
#' required column is recorded in \code{log} if one is given, and an error is raised.
#'
#' @param df A data frame.
#' @param cols Character vector of required column names.
#' @param what Character. Name of the file or table, used in the message.
#' @param log Optional issue log from \code{new_issue_log()}.
#' @param kind Character. \code{"file"} (default) or \code{"table"}; the word
#'   used after \code{what} in the message.
#'
#' @return Invisibly \code{TRUE}; errors otherwise.
#' @noRd
require_cols <- function(df, cols, what, log = NULL, kind = c("file", "table")) {
  kind <- match.arg(kind)
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    msg <- paste0("The ", what, " ", kind, " is missing required column(s): ",
                  paste0("`", missing, "`", collapse = ", "), ".")
    if (!is.null(log)) {
      log$add(msg)
    }
    stop(msg,
         "\nColumns found",
         if (kind == "file") " after name standardisation",
         ": ", paste(names(df), collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}



#' Missing optional columns will have all-NA
#' @noRd
ensure_cols <- function(df, cols, type = NA) {
  for (nm in setdiff(cols, names(df))) {
    df[[nm]] <- rep(type, nrow(df))
  }
  df
}




#' Coerce a raw column to Date (handling for POSIXct, bare numeric Excel serial or as character date formatting)
#' @noRd
as_date_safe <- function(x, col_name = "date", format = "%Y-%m-%d") {
  if (inherits(x, "Date")) {
    return(x)
  }
  # readxl reads Excel dates/datetimes as POSIXct in UTC; converting in any
  # other timezone can shift the calendar date by 1 day
  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }
  # a bare Excel serial number
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  # character, or an all-NA/blank column if cannot be parsed
  out <- as.Date(trimws(as.character(x)), format = format)
  unparsed <- is.na(out) & !is.na(x)
  if (any(unparsed)) {
    warning("Could not parse ", sum(unparsed), " value(s) in `", col_name,
            "` as a date (e.g. \"", as.character(x)[unparsed][1], "\"); set to NA.")
  }
  out
}





#' Coerce a raw yes/no column to logical
#' @noRd
as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(x != 0)
  }
  if (is.character(x)) {
    v <- tolower(trimws(x))
    out <- rep(NA, length(v))
    out[v %in% c("y", "yes", "true", "t", "1")] <- TRUE
    out[v %in% c("n", "no", "false", "f", "0", "")] <- FALSE
    return(out)
  }
  as.logical(x)
}




#' Collect data-quality problems
#' @noRd
new_issue_log <- function() {
  issues <- character(0)
  list(
    add = function(...) {
      issues <<- c(issues, paste0(...))
      invisible(NULL)
    },
    get = function() issues
  )
}





#' Stop with every accumulated data-quality issue, if there are any
#'
#' Each issue needs the user to correct the source file, so they are raised
#' together as one error: fix the source data and re-run until none remain.
#' @noRd
report_issues <- function(log) {
  issues <- log$get()
  if (length(issues) == 0) {
    return(invisible(NULL))
  }
  msg <- paste0(
    length(issues), " data-quality issue(s) found while building this unit:\n",
    paste0("  - ", issues, collapse = "\n"),
    "\nCorrect these in the source data and re-run."
  )
  stop(msg, call. = FALSE)
}




#' Test which cells of a column are blank (white-space) then trim. Empty cells are NA from \code{readxl}
#' @noRd
#'
is_blank_cell <- function(x) {
  if (is.character(x)) {
    is.na(x) | !nzchar(trimws(x))
  } else {
    is.na(x)
  }
}




#' Check rows that are missing a required value, log and drop. The row level equivalent for \code{require_cols()}
#' @noRd
check_required_cells <- function(df, cols, what, log, id_col = "patient_id",
                                 header_rows = 1L) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)))
  if (nrow(df) == 0) {
    return(df)
  }

  blank <- do.call(cbind, lapply(cols, function(cl) is_blank_cell(df[[cl]])))
  colnames(blank) <- cols
  bad <- which(rowSums(blank) > 0)
  if (length(bad) == 0) {
    return(df)
  }

  for (i in bad) {
    pid <- as.character(df[[id_col]][i])
    who <- if (is.na(pid) || !nzchar(trimws(pid))) {
      "no patient_id"
    } else {
      paste0("patient ", trimws(pid))
    }
    log$add("The ", what, " file, row ", i + header_rows, " (", who,
            "): missing required value(s) in ",
            paste(cols[blank[i, ]], collapse = ", "),
            "; row excluded from the unit.")
  }

  df[-bad, , drop = FALSE]
}





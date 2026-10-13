
# ingest_read contains helper functions for reading raw files,
# standardising names, map columns, etc


#' Standardise raw column names to snake_case
#' @noRd
standardise_names <- function(df) {
  nms <- trimws(names(df))                           # trim whitespace
  nms <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nms)   # camelCase -> camel_Case
  nms <- gsub("[^A-Za-z0-9]+", "_", nms)             # spaces/punctuation -> _
  nms <- tolower(nms)                                # lowercase
  nms <- gsub("_+", "_", nms)                        # collapse repeated _
  nms <- gsub("^_|_$", "", nms)                      # drop leading/trailing _
  names(df) <- nms                                   # set cleaned names
  df
}




#' Test a standardised column name against a keyword, returns TRUE or FALSE
#'
#' exact: name equals one of these will be TRUE straight away
#' all: name must contain every keyword
#' any: name must contain at least one keyword (ignored if NULL)
#' a rule with no all/any (and no exact hit) never matches
#' @noRd
matches_keywords <- function(name, rule) {
  if (name %in% rule$exact) {                         # exact name wins
    return(TRUE)
  }
  if (is.null(rule$all) && is.null(rule$any)) {       # nothing else to test
    return(FALSE)
  }
  has <- function(kw) grepl(kw, name, fixed = TRUE)   # literal substring check
  all_ok <- all(vapply(rule$all, has, logical(1)))    # every `all` keyword present
  any_ok <- is.null(rule$any) || any(vapply(rule$any, has, logical(1)))  # >=1 `any` keyword present
  all_ok && any_ok
}




#' Rename standardised column names to the names the ingest expects, after matching with keywords
#' @noRd
map_columns <- function(df, spec, what, log = NULL) {
  nms <- names(df)
  new_nms <- nms
  claimed <- nms %in% names(spec)     # columns already have a standard name are claimed

  for (key in setdiff(names(spec), nms)) {          # skip keys the file already has
    is_hit <- vapply(nms, matches_keywords, logical(1), rule = spec[[key]],
                     USE.NAMES = FALSE)             # which columns fit this key's rule and aren't already claimed
    hits <- which(is_hit & !claimed)
    if (length(hits) == 0) {                        # no match: leave key unmapped
      next
    }
    if (length(hits) > 1 && !is.null(log)) {        # warn, use the first
      log$add("The ", what, " file has ", length(hits), " columns that could be `",
              key, "` (", paste(nms[hits], collapse = ", "), "); used `",
              nms[hits[1]], "`. Rename the columns in the source file if that is wrong.",
              level = "warning")
    }
    new_nms[hits[1]] <- key                         # rename the first match to the standard name
    claimed[hits[1]] <- TRUE                        # so later keys can't claim it
  }

  names(df) <- new_nms
  df
}




#' Stop if required columns are absent; record the problem in the issue log if given
#' @noRd
require_cols <- function(df, cols, what, log = NULL, kind = c("file", "table")) {
  kind <- match.arg(kind)                            # "file"=raw sheet or "table"=unit tibble
  missing <- setdiff(cols, names(df))                # required names the data frame lacks
  if (length(missing) > 0) {
    msg <- paste0("The ", what, " ", kind, " is missing required column(s): ",
                  paste0("`", missing, "`", collapse = ", "), ".")
    if (!is.null(log)) {
      log$add(msg)                                   # log first
    }
    stop(msg,
         "\nColumns found",
         if (kind == "file") " after name standardisation",
         ": ", paste(names(df), collapse = ", "),    # show what was found
         call. = FALSE)
  }
  invisible(TRUE)
}




# Add any missing optional columns, filled with NA, so later code can rely on them
ensure_cols <- function(df, cols, type = NA) {
  for (nm in setdiff(cols, names(df))) {
    df[[nm]] <- rep(type, nrow(df))  # one NA per row
  }
  df
}




#' Coerce a raw column to Date, whatever format readxl delivered it in
#' @noRd
as_date_safe <- function(x, col_name = "date", format = "%Y-%m-%d") {
  if (inherits(x, "Date")) {                         # already a Date
    return(x)
  }
  if (inherits(x, "POSIXt")) {                       # Excel date/time: convert in UTC, or the
    return(as.Date(x, tz = "UTC"))                   # calendar day can shift by one
  }
  if (is.numeric(x)) {                               # bare Excel serial number
    return(as.Date(x, origin = "1899-12-30"))        # day-zero
  }
  out <- as.Date(trimws(as.character(x)), format = format)   # text dates, YYYY-MM-DD by default
  unparsed <- is.na(out) & !is_blank_cell(x)           # had a value but it didn't parse
  if (any(unparsed)) {
    warning("Could not parse ", sum(unparsed), " value(s) in `", col_name,
            "` as a date (e.g. \"", as.character(x)[unparsed][1], "\"); set to NA.")
  }
  out
}




#' Coerce a raw yes/no column to logical
#' @noRd
as_logical_safe <- function(x) {
  if (is.logical(x)) {                                   # leave as is if already logical
    return(x)
  }
  if (is.numeric(x)) {                                   # if numbers: 0 = FALSE, anything else = TRUE
    return(x != 0)
  }
  if (is.character(x)) {
    v <- tolower(trimws(x))                              # to lowercase and trim ws
    out <- rep(NA, length(v))                            # unrecognised text stays NA
    out[v %in% c("y", "yes", "true", "t", "1")] <- TRUE
    out[v %in% c("n", "no", "false", "f", "0", "")] <- FALSE   # blank cell counts as FALSE
    return(out)
  }
  as.logical(x)                                          # fallback for other types
}




#' Make an issue log: a small closure that collects messages while a unit is built
#' @noRd
new_issue_log <- function() {
  issues <- character(0)                       # message text
  kinds <- character(0)                        # "error" or "warning"
  list(
    add = function(..., level = "error") {
      level <- match.arg(level, c("error", "warning"))   # reject any other level
      issues <<- c(issues, paste0(...))        # <<- updates the log's own vectors, not a local copy
      kinds <<- c(kinds, level)
      invisible(NULL)
    },
    get = function(level = NULL) {
      if (is.null(level)) issues else issues[kinds %in% level]   # all messages, or one level only
    }
  )
}





#' Stop the build with all collected data-quality errors at once (warnings are ignored here)
#' @noRd
report_issues <- function(log) {
  issues <- log$get("error")                         # error-level messages only
  if (length(issues) == 0) {                         # carry on silently
    return(invisible(NULL))
  }
  msg <- paste0(                                     # one bulleted message listing every issue
    length(issues), " data-quality issue(s) found while building this unit:\n",
    paste0("  - ", issues, collapse = "\n"),
    "\nCorrect these in the source data and re-run."
  )
  # R truncates error text longer than options(warning.length, default 1000 bytes); so leave ~150 bytes of headroom
  if (nchar(msg, type = "bytes") > getOption("warning.length", 1000L) - 150L) {
    message(msg)                                     # print the full list as a message instead...
    msg <- paste0(                                   # and make the error itself a short pointer
      length(issues), " data-quality issue(s) found while building this unit; ",
      "the full list is printed above. Correct them in the source data and re-run."
    )
  }
  stop(msg)
}




#' TRUE for each cell that is empty: NA, or "" (for text columns) or only whitespace
#' @noRd
is_blank_cell <- function(x) {
  if (is.character(x)) {
    is.na(x) | !nzchar(trimws(x))   # NA, or nothing left after trimming
  } else {
    is.na(x)                        # numbers, dates etc. can only be blank as NA
  }
}




#' Row-level version of \code{require_cols()}: log and drop rows with a blank required cell
#' @noRd
check_required_cells <- function(df, cols, what, log, id_col = "patient_id",
                                 header_rows = 1L) {
  stopifnot(is.data.frame(df), all(cols %in% names(df)))
  if (nrow(df) == 0) {
    return(df)
  }

  blank <- do.call(cbind, lapply(cols, function(cl) is_blank_cell(df[[cl]])))  # rows x cols matrix of TRUE = blank
  colnames(blank) <- cols
  bad <- which(rowSums(blank) > 0)    # rows with at least one blank required cell
  if (length(bad) == 0) {
    return(df)
  }

  for (i in bad) {
    pid <- as.character(df[[id_col]][i])
    who <- if (is.na(pid) || !nzchar(trimws(pid))) {      # name the patient if the ID is present
      "no patient_id"
    } else {
      paste0("patient ", trimws(pid))
    }
    log$add("The ", what, " file, row ", sheet_row(df, i, header_rows), " (", who,
            "): missing required value(s) in ",
            paste(cols[blank[i, ]], collapse = ", "),     # which columns were blank in this row
            "; row not checked further, fill in the value(s) in the source file.")
  }

  df[-bad, , drop=FALSE]      # drop bad rows
}




#' Row number of a data-frame row as it appears in the source sheet, earlier checks may drop rows
#' @noRd
sheet_row <- function(df, i, header_rows = 1L) {
  if (".sheet_row" %in% names(df)) df$.sheet_row[i] else i + header_rows
}



#' NHI format: three letters then four digits, case-insensitive (ABC1234)
#' @noRd
is_valid_nhi <- function(x) {
  x <- toupper(as.character(x))
  !is.na(x) & grepl("^[A-Z]{3}[0-9]{4}$", x)
}



#' Log and drop rows whose patient_id isn't a valid NHI (blank IDs are left to \code{check_required_cells})
#' @noRd
check_patient_ids <- function(df, what, log, id_col = "patient_id", header_rows = 1L) {
  stopifnot(is.data.frame(df), id_col %in% names(df))
  if (nrow(df) == 0) {
    return(df)
  }

  ids <- trimws(as.character(df[[id_col]]))
  blank <- is.na(ids) | !nzchar(ids)
  bad <- which(!blank & !is_valid_nhi(ids))          # present but malformed
  if (length(bad) == 0) {
    df[[id_col]] <- toupper(ids)                     # normalise valid IDs to upper case
    return(df)
  }

  for (i in bad) {
    log$add("The ", what, " file, row ", sheet_row(df, i, header_rows), ": `", ids[i],
            "` is not a valid NHI number (three letters then four digits, e.g. ",
            "ABC1234); row not checked further, correct the patient_id in the source file.")
  }

  df[[id_col]] <- toupper(ids)
  df[-bad, , drop=FALSE]        # drop bad rows
}




#' Log and drop PE rows for a patient who has no row in the A3 file
#' @noRd
check_known_patients <- function(df, known_ids, what, log, id_col = "patient_id",
                                 header_rows = 1L) {
  stopifnot(is.data.frame(df), id_col %in% names(df))
  if (nrow(df) == 0) {
    return(df)
  }

  ids <- trimws(as.character(df[[id_col]]))
  bad <- which(!is.na(ids) & nzchar(ids) & !(toupper(ids) %in% toupper(known_ids)))     # case-insensitive lookup
  if (length(bad) == 0) {
    return(df)
  }

  for (i in bad) {
    log$add("The ", what, " file, row ", sheet_row(df, i, header_rows), ": patient ", ids[i],
            " has no row in the unit (A3) file, so this episode cannot be ",
            "attached to a catheter; row not checked further, check the patient_id ",
            "or add the patient to the unit (A3) file.")
  }
  df[-bad, , drop=FALSE]             # drop bad rows
}

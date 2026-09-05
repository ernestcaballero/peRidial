
# ingest.R -- read a PD unit's raw Excel exports and build the pd_unit object
#
# The build runs in one direction:
#
#   raw A3 / PE files
#     -> raw_patients / raw_catheters / raw_pe_episodes  (tidied tables)
#     -> tau per patient                                 (censoring)
#     -> pd_infection objects, chained per patient       (episode_type)
#     -> matched to the catheter active on infection_date
#     -> pd_catheter objects, each owning its episodes
#     -> pd_patient objects, each owning its catheters
#     -> cohort filter to [t0, t1]
#     -> flat tibbles + headline counts
#     -> new_pd_unit() -> validate_pd_unit()
#
# The three tibbles on the finished object are flattened from the object graph,
# not carried over from the raw files



#' INPUT HELPERS
#' Standardise raw column names to snake_case
#'
#' Raw A3/PE exports arrive with names like "Patient ID", "PD Start Date" or
#' "dateOfBirth". This normalises them to the snake_case names the rest of the
#' ingest expects.
#'
#' @param df A data frame.
#'
#' @return \code{df} with standardised names.
#' @noRd
#'
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


#' Error if a raw file is missing columns the ingest cannot proceed without
#'
#' @param df A data frame.
#' @param cols Character vector of required column names.
#' @param what Character. Label for the file, used in the error message.
#'
#' @return Invisibly \code{TRUE}; errors otherwise.
#' @noRd
#'
require_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("The ", what, " file is missing required column(s): ",
         paste(missing, collapse = ", "),
         ".\nColumns found after name standardisation: ",
         paste(names(df), collapse = ", "))
  }
  invisible(TRUE)
}


#' Add any missing optional columns as all-NA
#'
#' Lets the ingest reference optional fields unconditionally without a
#' \code{select()} failing on a file that happens not to carry them.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to guarantee.
#' @param type A prototype value used to fill missing columns.
#'
#' @return \code{df}, with any missing column added and filled with \code{type}.
#' @noRd
#'
ensure_cols <- function(df, cols, type = NA) {
  for (nm in setdiff(cols, names(df))) {
    df[[nm]] <- rep(type, nrow(df))
  }
  df
}


#' Coerce a raw column to Date, whatever readxl handed back
#'
#' \code{readxl} returns date-ish columns as \code{POSIXct}, as a bare
#' numeric Excel serial, or as character, depending on how the cell was
#' formatted. Plain \code{as.Date(x, format = "%Y-%m-%d")} silently ignores
#' \code{format} for the first of those and applies the local timezone,
#' which can shift the calendar date by a day -- so this branches on the
#' incoming type instead of coercing blindly. Text values are expected as
#' ISO (\code{YYYY-MM-DD}); pass \code{format} to change that.
#'
#' @param x A vector to coerce.
#' @param col_name Character. Column name, used in the warning if a value
#'   can't be parsed.
#' @param format Character. \code{strptime()}-style format for text dates.
#'   Defaults to ISO (\code{"%Y-%m-%d"}).
#'
#' @return A \code{Date} vector the same length as \code{x}.
#' @noRd
#'
as_date_safe <- function(x, col_name = "date", format = "%Y-%m-%d") {
  if (inherits(x, "Date")) {
    return(x)
  }
  # readxl reads Excel dates/datetimes as POSIXct in UTC; converting in any
  # other timezone can shift the calendar date by a day
  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }
  # a bare Excel serial number; the 1900 date system's origin is 1899-12-30
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  # character, or an all-NA/blank column if cannot be parsed
  out <- as.Date(trimws(as.character(x)), format = format)
  unparsed <- is.na(out) & !is.na(x)
  if (any(unparsed)) {
    warning("Could not parse ", sum(unparsed), " value(s) in `", col_name,
            "` as a date (e.g. \"", as.character(x)[unparsed][1], "\"); set to NA.",
            call. = FALSE)
  }
  out
}


#' Coerce a raw yes/no column to logical
#'
#' Excel forms record flags as TRUE/FALSE, 1/0, or "Yes"/"No" depending on the
#' export. \code{dplyr::coalesce(x, FALSE)} errors outright on the character
#' form, so normalise first.
#'
#' @param x A vector to coerce.
#'
#' @return A logical vector the same length as \code{x}.
#' @noRd
#'
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
#'
#' A registry extract of accumulated gaps and data-quality issues.
#' Reported at the end of data ingestion.
#'
#' @return A list of functions: \code{add(...)} records a message,
#'   \code{get()} returns them all.
#' @noRd
#'
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


#' Display accumulated data-quality issues
#'
#' @param log An issue log from \code{new_issue_log()}.
#' @param strict Logical. \code{TRUE} to raise an error rather than a warning.
#'
#' @return Invisibly \code{NULL}.
#' @noRd
#'
report_issues <- function(log, strict = FALSE) {
  issues <- log$get()
  if (length(issues) == 0) {
    return(invisible(NULL))
  }
  msg <- paste0(
    length(issues), " data-quality issue(s) found while building this unit:\n",
    paste0("  - ", issues, collapse = "\n"),
    "\nCorrect these in the source data and re-run."
  )
  if (strict) {
    stop(msg)
  }
  warning(msg)
  invisible(NULL)
}



# create catheter_id

#' Generate catheter_id values from patient_id and insertion_date
#'
#' A patient can have more than one PD catheter over time. This builds
#' \code{catheter_id} as \code{patient_id} plus a two-digit sequence number
#' ordered by \code{insertion_date} within each patient (e.g. a patient
#' \code{"XYZ1234"}'s first catheter becomes \code{"XYZ1234_01"}, their next
#' one (if any) \code{"XYZ1234_02"}, and so on).
#'
#' @param patient_id Character vector. Patient identifier for each row (NHI).
#' @param insertion_date Date vector, the same length as \code{patient_id}.
#'   Date each row's catheter was inserted; used to order multiple catheters
#'   within the same patient.
#'
#' @return Character vector, same length and order as the inputs, giving each
#'   row's generated \code{catheter_id}.
#' @noRd
#'
create_catheter_id <- function(patient_id, insertion_date) {
  stopifnot(length(patient_id) == length(insertion_date))
  stopifnot(inherits(insertion_date, "Date"))

  catheter_id <- rep(NA_character_, length(patient_id))

  for (pid in unique(patient_id[!is.na(patient_id)])) {
    rows <- which(patient_id == pid)
    # NAs sort last, so a catheter with no insertion_date gets the highest
    # sequence number rather than silently displacing a dated one
    ord <- rows[order(insertion_date[rows], na.last = TRUE)]
    catheter_id[ord] <- paste0(pid, "_", sprintf("%02d", seq_along(ord)))
  }

  catheter_id
}




#' Find a patient's transplant date from removal reason
#'
#' Transplant is a censoring event. When that field is blank, this falls back to a
#' catheter whose \code{removal_reason} names a transplant, using that
#' catheter's \code{pd_stop_date} as the censoring date instead.
#'
#' @param demo One-row data frame of this patient's demographic fields.
#' @param cath Data frame of this patient's catheter rows.
#'
#' @return A single \code{Date}, or \code{NA}.
#' @noRd
#'
find_transplant_date <- function(demo, cath) {
  if (nrow(demo) > 0 && !is.na(demo$transplant_date[1])) {
    return(as_date_safe(demo$transplant_date[1], "transplant_date"))
  }
  # fallback: a catheter removed because the patient was transplanted
  if ("removal_reason" %in% names(cath) && nrow(cath) > 0) {
    hit <- grepl("transplant", tolower(as.character(cath$removal_reason)))
    hit <- hit & !is.na(cath$pd_stop_date)
    if (any(hit)) {
      return(min(cath$pd_stop_date[hit]))
    }
  }
  as.Date(NA)
}



#' Derive a patient's censoring point (tau)
#'
#' \eqn{tau} is the date a patient permanently stopped being at risk of PD
#' peritonitis. Four events end PD, and the earliest of them wins:
#'
#' \itemize{
#'   \item \strong{Death}: \code{date_of_death} on the A3 form.
#'   \item \strong{Transplant}: see \code{find_transplant_date()}.
#'   \item \strong{Permanent transfer to HD}: of the four
#'     \code{dialysis_modality_change} values only "Any PD to HD" takes a
#'     patient off PD; CAPD<->APD swaps stay within PD and "HD to any PD" is a
#'     return to it.
#'   \item \strong{PD stopped with no successor catheter}: every catheter
#'     has closed and none reopened, so the patient is off PD even though no
#'     explicit modality-change row says so. Controlled by
#'     \code{censor_on_last_stop}, since a patient on a genuine break from PD
#'     looks identical in the data.
#' }
#'
#' \code{transfer_reason} is set to the canonical event category, so it is
#' comparable across patients. The free-text clinical detail (e.g. "reduced
#' ultrafiltration") is returned separately as \code{detail} and carried into
#' the unit's \code{patients} tibble.
#'
#' @param demo One-row data frame of this patient's demographic fields.
#' @param mod Data frame of this patient's modality-change rows.
#' @param cath Data frame of this patient's catheter rows.
#' @param censor_on_last_stop Logical. Apply the fourth rule above.
#'
#' @return A list with \code{reason} (character), \code{date} (Date),
#'   \code{detail} (character), \code{transplant_gap} (logical: \code{TRUE}
#'   when a "transplant" modality change was recorded but no date could be
#'   found for it anywhere) and \code{hd_transfer_gap} (logical: \code{TRUE}
#'   when an "Any PD to HD" modality change was recorded but none of those
#'   rows had a usable \code{date_modality_change}).
#' @noRd
#'
PD_TO_HD <- "any pd to hd"

derive_patient_tau <- function(demo, mod, cath, censor_on_last_stop = TRUE) {
  dates <- as.Date(character(0))
  reasons <- character(0)
  # details <- character(0)

  # (1) death
  if ("date_of_death" %in% names(demo) && nrow(demo) > 0 &&
      !is.na(demo$date_of_death[1])) {
    dates <- c(dates, demo$date_of_death[1])
    reasons <- c(reasons, "death")
    detail <- if ("cause_of_death" %in% names(demo)) {
      as.character(demo$cause_of_death[1])
    } else {
      NA_character_
    }
    details <- c(details, detail)
  }

  # (2) transplant
  tx <- find_transplant_date(demo, cath)
  if (!is.na(tx)) {
    dates <- c(dates, tx)
    reasons <- c(reasons, "transplant")
    details <- c(details, NA_character_)
  }

  # a modality-change row can flag a transplant even when no date is available
  transplant_row <- nrow(mod) > 0 &&
    any(tolower(trimws(as.character(mod$dialysis_modality_change))) == "transplant")
  transplant_gap <- transplant_row && is.na(tx)

  # (3) permanent transfer to HD
  hd_transfer_gap <- FALSE
  if (nrow(mod) > 0) {
    hd_rows <- which(tolower(trimws(as.character(mod$dialysis_modality_change))) == PD_TO_HD)
    to_hd <- hd_rows[!is.na(mod$date_modality_change[hd_rows])]
    if (length(to_hd) > 0) {
      last_hd <- to_hd[which.max(mod$date_modality_change[to_hd])]
      dates <- c(dates, mod$date_modality_change[last_hd])
      reasons <- c(reasons, "permanent transfer to HD")
      details <- c(details, trimws(as.character(mod$modality_change_reason[last_hd])))
    } else if (length(hd_rows) > 0) {
      hd_transfer_gap <- TRUE
    }
  }

  if (length(dates) > 0) {
    i <- which.min(dates)
    return(list(reason = reasons[i], date = dates[i], detail = details[i],
                transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap))
  }

  # (4) PD stopped with no successor catheter
  if (censor_on_last_stop && nrow(cath) > 0 && !anyNA(cath$pd_stop_date)) {
    last <- which.max(cath$pd_stop_date)
    detail <- if ("removal_reason" %in% names(cath)) {
      as.character(cath$removal_reason[last])
    } else {
      NA_character_
    }
    return(list(reason = "pd stopped",
                date = cath$pd_stop_date[last],
                detail = detail,
                transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap))
  }

  list(reason = NA_character_, date = as.Date(NA), detail = NA_character_,
       transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap)
}



#' Was this patient on PD at any point during the reporting period?
#'
#' Defines the reporting cohort: a patient counts towards \code{n_patients} if
#' any of their catheter windows, censored at \code{tau}, overlaps
#' \code{[t0, t1]}. This is the same cohort \code{total_patient_years()}
#' accrues over, so the peritonitis rate's numerator and denominator agree.
#'
#' @param cath Data frame of this patient's catheter rows.
#' @param t0,t1 Dates bounding the reporting period.
#' @param tau Date the patient left PD, or \code{NA}.
#'
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
#'
on_pd_in_period <- function(cath, t0, t1, tau = as.Date(NA)) {
  if (nrow(cath) == 0) {
    return(FALSE)
  }
  starts <- cath$pd_start_date
  stops <- cath$pd_stop_date
  if (!is.na(tau)) {
    stops[is.na(stops) | stops > tau] <- tau
  }
  stops[is.na(stops)] <- t1   # still active at the end of the period
  any(!is.na(starts) & starts <= t1 & stops >= t0)
}




#' Flatten patient_list into the unit's patients tibble
#'
#' @param patient_list List of \code{pd_patient} objects.
#' @param t0,t1 Dates bounding the reporting period.
#' @param details Named character vector of transfer details, keyed by
#'   patient_id.
#'
#' @return A tibble, one row per patient.
#' @noRd
#'
patients_to_tibble <- function(patient_list, t0, t1) {
  if (length(patient_list) == 0) {
    return(tibble::tibble(
      patient_id = character(0), t0 = as.Date(character(0)),
      t1 = as.Date(character(0)), new_patient_flag = logical(0),
      n_catheters = integer(0), n_episodes = numeric(0),
      transfer_reason = character(0), transfer_date = as.Date(character(0)),
      # transfer_detail = character(0),
      first_pd_start_date = as.Date(character(0)),
      patient_years_at_risk = numeric(0)
    ))
  }

  # computed before tibble(), which evaluates its arguments in a data mask
  ids <- vapply(patient_list, function(p) p$patient_id, character(1))

  first_start <- do.call(c, lapply(patient_list, function(p) {
    starts <- do.call(c, lapply(p$catheters, function(cath) cath$pd_start_date))
    if (is.null(starts) || length(starts) == 0) as.Date(NA) else min(starts)
  }))

  # per-patient contribution to the unit's tpyar denominator
  py <- vapply(patient_list, function(p) total_patient_years(list(p), t0, t1),
               numeric(1))

  tibble::tibble(
    patient_id = ids,
    t0 = rep(t0, length(patient_list)),
    t1 = rep(t1, length(patient_list)),
    new_patient_flag = vapply(patient_list,
                              function(p) as.logical(p$new_patient_flag),
                              logical(1)),
    n_catheters = vapply(patient_list,
                         function(p) as.integer(p$n_catheters), integer(1)),
    n_episodes = vapply(patient_list,
                        function(p) as.numeric(p$n_episodes), numeric(1)),
    transfer_reason = vapply(patient_list,
                             function(p) as.character(p$transfer_reason),
                             character(1)),
    transfer_date = do.call(c, lapply(patient_list, function(p) p$transfer_date)),
    # transfer_detail = unname(ifelse(ids %in% names(details),
    #                                 details[ids], NA_character_)),
    first_pd_start_date = first_start,
    patient_years_at_risk = py
  )
}


#' Flatten every catheter in patient_list into the unit's catheters tibble
#'
#' @param patient_list List of \code{pd_patient} objects.
#' @param t0,t1 Dates bounding the reporting period.
#'
#' @return A tibble, one row per catheter.
#' @noRd
#'
catheters_to_tibble <- function(patient_list, t0, t1) {
  caths <- unlist(lapply(patient_list, function(p) p$catheters),
                  recursive = FALSE, use.names = FALSE)

  if (length(caths) == 0) {
    return(tibble::tibble(
      patient_id = character(0), catheter_id = character(0),
      insertion_date = as.Date(character(0)), procedure_type = character(0),
      pd_start_date = as.Date(character(0)), pd_stop_date = as.Date(character(0)),
      removal_reason = character(0), n_peritonitis_episodes = numeric(0),
      peritonitis_flag = logical(0), exposure_days_in_period = numeric(0)
    ))
  }

  # each catheter's tau, so exposure days are censored the same way tpyar is
  taus <- unlist(lapply(patient_list, function(p) {
    rep(list(p$transfer_date), length(p$catheters))
  }), recursive = FALSE, use.names = FALSE)

  # computed before tibble(), which evaluates its arguments in a data mask
  exposure <- vapply(seq_along(caths), function(i) {
    catheter_exposure_days(caths[[i]], t0, t1, tau = taus[[i]])
  }, numeric(1))

  tibble::tibble(
    patient_id = vapply(caths, function(z) z$patient_id, character(1)),
    catheter_id = vapply(caths, function(z) z$catheter_id, character(1)),
    insertion_date = do.call(c, lapply(caths, function(z) z$insertion_date)),
    procedure_type = vapply(caths, function(z) as.character(z$procedure_type),
                            character(1)),
    pd_start_date = do.call(c, lapply(caths, function(z) z$pd_start_date)),
    pd_stop_date = do.call(c, lapply(caths, function(z) z$pd_stop_date)),
    removal_reason = vapply(caths, function(z) as.character(z$removal_reason),
                            character(1)),
    n_peritonitis_episodes = vapply(caths,
                                    function(z) as.numeric(z$n_peritonitis_episodes),
                                    numeric(1)),
    peritonitis_flag = vapply(caths, function(z) as.logical(z$peritonitis_flag),
                              logical(1)),
    exposure_days_in_period = exposure
  )
}


#' Flatten every peritonitis episode into the unit's infections tibble
#'
#' Walks patient -> catheter -> infections, so each episode row carries the
#' \code{catheter_id} of the catheter that owns it. That column is what
#' \code{validate_pd_unit()} checks referential integrity against.
#'
#' @param patient_list List of \code{pd_patient} objects.
#' @param t0,t1 Dates bounding the reporting period.
#'
#' @return A tibble, one row per episode.
#' @noRd
#'
infections_to_tibble <- function(patient_list, t0, t1) {
  rows <- list()

  for (p in patient_list) {
    for (cath in p$catheters) {
      for (inf in cath$infections) {
        # an episode counts towards the rate if it is inside this catheter's active window within [t0, t1] and is not a relapse
        counts <- count_episodes_in_period(list(inf), t0, t1,
                                           cath$pd_start_date,
                                           cath$pd_stop_date) > 0
        rows[[length(rows) + 1]] <- tibble::tibble(
          patient_id = inf$patient_id,
          catheter_id = cath$catheter_id,
          infection_date = inf$infection_date,
          episode_type = as.character(inf$episode_type),
          organisms = paste(unlist(inf$organism_list), collapse = ", "),
          n_organisms = length(inf$organism_list),
          last_dose_antibiotic = inf$last_dose_antibiotic,
          outcome = as.character(inf$outcome),
          outcome_date = inf$outcome_date,
          counts_toward_rate = counts
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      patient_id = character(0), catheter_id = character(0),
      infection_date = as.Date(character(0)), episode_type = character(0),
      organisms = character(0), n_organisms = integer(0),
      last_dose_antibiotic = as.Date(character(0)), outcome = character(0),
      outcome_date = as.Date(character(0)), counts_toward_rate = logical(0)
    ))
  }

  out <- do.call(rbind, rows)
  out[order(out$patient_id, out$infection_date), ]
}




#' USER FACING BUILDER
#'
#' Build a pd_unit object from raw data files
#'
#' Reads a unit's raw dialysis (A3) file and peritonitis-episode (PE) file and
#' builds the full nested object graph for a reporting period:
#' peritonitis episodes owned by the catheter that was active when they occurred,
#' catheters owned by their patient, and every patient who was on PD at any
#' point in \code{[t0, t1]} collected into one \code{pd_unit}.
#'
#' The returned object carries both the nested \code{patient_list} and three
#' flat tibbles (\code{patients}, \code{catheters}, \code{infections})
#' derived from it, plus the headline numbers the ISPD indicators need:
#' \code{n_new}, \code{n_patients} and \code{tpyar}.
#'
#' Data-quality problems (a missing modality-change reason, an episode that
#' can't be attached to a catheter, an organism-free episode) are collected
#' across the whole file and reported together as a single warning, rather
#' than aborting on the first one. Set \code{strict = TRUE} to make them an
#' error instead.
#'
#' @param unit_data_path Character. Path to the raw unit (A3) Excel file
#'   containing both patient and catheter data.
#' @param infection_data_path Character. Path to the raw infection/peritonitis
#'   episode (PE) Excel file.
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param unit_id Character. Identifier for the unit, e.g.
#'   \code{"Auckland PD Unit"}. Defaults to \code{NA_character_}.
#' @param censor_on_last_stop Logical. Treat a patient whose every catheter
#'   has closed, with none reopened, as having left PD on the last
#'   \code{pd_stop_date}. Defaults to \code{TRUE}. Set \code{FALSE} if your
#'   unit records genuine breaks from PD, since a break and a permanent exit
#'   look identical in the raw data.
#' @param strict Logical. Raise collected data-quality issues as an error
#'   rather than a warning. Defaults to \code{FALSE}.
#'
#' @return A validated \code{pd_unit} object.
#' @seealso \code{\link{new_pd_unit}()} for the underlying constructor.
#' @export
#'
#' @examples
#' \dontrun{
#' auckland <- pd_unit(
#'   unit_data_path      = "data-raw/a3_2025.xlsx",
#'   infection_data_path = "data-raw/pe_2025.xlsx",
#'   t0      = as.Date("2025-01-01"),
#'   t1      = as.Date("2025-12-31"),
#'   unit_id = "Auckland PD Unit"
#' )
#' auckland
#' auckland$tpyar
#' }
pd_unit <- function(unit_data_path,
                    infection_data_path,
                    t0,
                    t1,
                    unit_id = NA_character_,
                    censor_on_last_stop = TRUE,
                    strict = FALSE) {

  stopifnot(inherits(t0, "Date"), length(t0) == 1, !is.na(t0))
  stopifnot(inherits(t1, "Date"), length(t1) == 1, !is.na(t1))
  if (t0 > t1) {
    stop("t0 must be on or before t1.", call. = FALSE)
  }

  log <- new_issue_log()

  # Read file and standardise
  raw_a3 <- standardise_names(readxl::read_excel(unit_data_path))
  raw_pe_file <- standardise_names(readxl::read_excel(infection_data_path))

  require_cols(raw_a3, c("patient_id", "insertion_date", "pd_start_date"),
               "unit (A3)")
  require_cols(raw_pe_file, c("patient_id", "date_of_infection", "organism"),
               "infection (PE)")

  a3_optional <- c("date_of_birth", "gender", "ethnicity",
                   "primary_kidney_disease", "height", "weight",
                   "cigarette_smoking_status", "diabetes_type",
                   "dialysis_modality_change", "modality_change_reason",
                   "date_modality_change", "date_of_death", "cause_of_death",
                   "transplant_date", "dialysis_type",
                   "procedure_type", "pd_stop_date", "removal_reason")
  raw_a3 <- ensure_cols(raw_a3, a3_optional)

  pe_optional <- c("last_dose_antibiotic", "overnight_hospitalisation", "days_hospitalised",
                   "catheter_removed", "catheter_removed_date", "interim_hd",
                   "permanent_hd", "first_dialysis_date", "last_dialysis_date")
  raw_pe_file <- ensure_cols(raw_pe_file, pe_optional)

  # tidy the A3 form into patients, catheters and modality changes
  raw_a3 <- raw_a3 |>
    dplyr::mutate(
      patient_id = trimws(as.character(patient_id)),
      date_of_birth = as_date_safe(date_of_birth, "date_of_birth"),
      date_of_death = as_date_safe(date_of_death, "date_of_death"),
      date_modality_change = as_date_safe(date_modality_change,"date_modality_change"),
      insertion_date = as_date_safe(insertion_date, "insertion_date"),
      pd_start_date = as_date_safe(pd_start_date, "pd_start_date"),
      pd_stop_date = as_date_safe(pd_stop_date, "pd_stop_date")
    )

  raw_patients <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "date_of_birth", "gender", "ethnicity",
      "primary_kidney_disease", "height", "weight",
      "cigarette_smoking_status", "diabetes_type", "date_of_death",
      "cause_of_death", "dialysis_type", "transplant_date"))) |>
    dplyr::distinct(patient_id, .keep_all = TRUE)

  raw_catheters <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "insertion_date", "procedure_type", "pd_start_date",
      "pd_stop_date", "removal_reason"))) |>
    dplyr::filter(!is.na(patient_id)) |>
    dplyr::mutate(catheter_id = create_catheter_id(patient_id, insertion_date))

  raw_modality <- raw_a3 |>
    dplyr::select(dplyr::any_of(c(
      "patient_id", "dialysis_modality_change", "modality_change_reason", "date_modality_change")))

  # tidy the PE form into episodes
  raw_pe_episodes <- raw_pe_file |>
    dplyr::mutate(
      patient_id = trimws(as.character(patient_id)),
      date_of_infection = as_date_safe(date_of_infection, "date_of_infection"),
      last_dose_antibiotic = as_date_safe(last_dose_antibiotic, "last_dose_antibiotic"),
      catheter_removed_date = as_date_safe(catheter_removed_date, "catheter_removed_date"),
      first_dialysis_date = as_date_safe(first_dialysis_date, "first_dialysis_date"),
      last_dialysis_date = as_date_safe(last_dialysis_date, "last_dialysis_date"),
      catheter_removed = as_logical_safe(catheter_removed), # normalise the yes/no flags before case_when() combines them
      permanent_hd = as_logical_safe(permanent_hd),
      interim_hd = as_logical_safe(interim_hd),
      overnight_hospitalisation = as_logical_safe(overnight_hospitalisation)
    ) |>
    dplyr::mutate(
      # each element must itself be a list: new_pd_infection() and get_episode_type() both require is.list(organism_list)
      organism_list = lapply(strsplit(as.character(organism), ",\\s*"),
                             function(z) as.list(trimws(z))),
      # Outcome priority is catheter removal > permanent HD > temporary HD > hospitalisation; the most severe flag set wins.
      outcome = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE)          ~ "catheter removed",
        dplyr::coalesce(permanent_hd, FALSE)              ~ "permanent transfer to HD",
        dplyr::coalesce(interim_hd, FALSE)                ~ "temporary transfer to HD",
        dplyr::coalesce(overnight_hospitalisation, FALSE) ~ "hospitalisation",
        TRUE ~ NA_character_
      ),
      # outcome/outcome_date are deliberately nullable: an episode that
      # resolved without any of the above falls through to NA, which validate_pd_infection() reads as implied good recovery.
      outcome_date = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE) ~ catheter_removed_date,
        dplyr::coalesce(permanent_hd, FALSE) |
          dplyr::coalesce(interim_hd, FALSE)     ~ first_dialysis_date,
        TRUE ~ as.Date(NA)
      )
    ) |>
    dplyr::arrange(patient_id, date_of_infection)

  # Derive tau per patient, then censor the catheter windows
  pids <- sort(unique(raw_catheters$patient_id))
  taus <- list()
  for (pid in pids) {
    taus[[pid]] <- derive_patient_tau(
      demo = raw_patients[raw_patients$patient_id == pid, , drop = FALSE],
      mod  = raw_modality[raw_modality$patient_id == pid, , drop = FALSE],
      cath = raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE],
      censor_on_last_stop = censor_on_last_stop
    )
    tau <- taus[[pid]]$date

    # tau is the authority on when PD ended: close any catheter left open past it,
    # and pull back any that claims to have run on beyond it
    if (!is.na(tau)) {
      rows <- which(raw_catheters$patient_id == pid)
      open <- rows[is.na(raw_catheters$pd_stop_date[rows])]
      if (length(open) > 0) {
        raw_catheters$pd_stop_date[open] <- tau
      }
      late <- rows[!is.na(raw_catheters$pd_stop_date[rows]) &
                     raw_catheters$pd_stop_date[rows] > tau]
      if (length(late) > 0) {
        log$add("Patient ", pid, ": catheter(s) ",
                paste(raw_catheters$catheter_id[late], collapse = ", "),
                " have a pd_stop_date after this patient's ",
                taus[[pid]]$reason, " on ", format(tau),
                "; clipped to that date.")
        raw_catheters$pd_stop_date[late] <- tau
      }
    }

    # a PD-to-HD transfer with no reason recorded is a gap in the source data
    if (identical(taus[[pid]]$reason, "permanent transfer to HD")) {
      detail <- taus[[pid]]$detail
      if (is.na(detail) || !nzchar(detail)) {
        log$add("Patient ", pid, " has an 'Any PD to HD' modality change on ",
                format(taus[[pid]]$date),
                " with no modality_change_reason.")
      }
    }

    # a 'transplant' modality change with no date is a gap: the date can only come from transplant_date
    if (isTRUE(taus[[pid]]$transplant_gap)) {
      log$add("Patient ", pid, " has a 'transplant' modality change recorded ",
              "but no transplant_date supplied (and no catheter ",
              "removal_reason mentioning transplant); this patient's true ",
              "censoring date is unknown and they are being treated as if ",
              "still active on PD.")
    }

    # for 'Any PD to HD': the modality change was recorded the row is missing date_modality_change
    if (isTRUE(taus[[pid]]$hd_transfer_gap)) {
      log$add("Patient ", pid, " has an 'Any PD to HD' modality change ",
              "recorded but no date_modality_change supplied for it; this ",
              "patient's true censoring date is unknown and they are being ",
              "treated as if still active on PD.")
    }
  }


  # Build pd_infection objects, chained per patient
  # get_episode_type() classifies each episode against the patient's
  # immediately preceding one, which can span a catheter change, so the chain
  # has to be walked per patient in date order.
  build_patient_infections <- function(df) {
    infections <- list()
    prior <- NULL
    for (i in seq_len(nrow(df))) {
      inf <- tryCatch(
        pd_infection(
          patient_id = df$patient_id[i],
          infection_date = df$date_of_infection[i],
          organism_list = df$organism_list[[i]],
          last_dose_antibiotic = df$last_dose_antibiotic[i],
          outcome = df$outcome[i],
          outcome_date = df$outcome_date[i],
          prior_episode = prior
        ),
        error = function(e) {
          log$add("Patient ", df$patient_id[i], ", episode on ",
                  format(df$date_of_infection[i]), ": ", conditionMessage(e),
                  " (episode skipped)")
          NULL
        }
      )
      if (!is.null(inf)) {
        infections[[length(infections) + 1]] <- inf
        prior <- inf
      }
    }
    infections
  }

  infections_by_patient <- if (nrow(raw_pe_episodes) > 0) {
    lapply(split(raw_pe_episodes, raw_pe_episodes$patient_id),
           build_patient_infections)
  } else {
    list()
  }

  # Match each episode to the catheter active on its infection_date
  match_active_catheter_id <- function(pid, infection_date) {
    cath <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
    if (nrow(cath) == 0) {
      return(NA_character_)
    }
    active <- !is.na(cath$pd_start_date) &
      cath$pd_start_date <= infection_date &
      (is.na(cath$pd_stop_date) | infection_date <= cath$pd_stop_date)
    candidates <- cath$catheter_id[active]

    if (length(candidates) == 0) {
      log$add("Patient ", pid, ": no active PD catheter on infection_date ",
              format(infection_date),
              "; this episode is not attached to a catheter and is excluded ",
              "from the unit.")
      return(NA_character_)
    }
    if (length(candidates) > 1) {
      log$add("Patient ", pid, ": ", length(candidates),
              " overlapping PD catheters active on infection_date ",
              format(infection_date), " (",
              paste(candidates, collapse = ", "),
              "); earliest used. Correct the insertion_date/pd_start_date/",
              "pd_stop_date of these catheters so their windows don't overlap.")
    }
    candidates[1]
  }

  all_infections <- unlist(infections_by_patient, recursive = FALSE,
                           use.names = FALSE)
  if (is.null(all_infections)) all_infections <- list()

  infections_by_catheter <- if (length(all_infections) > 0) {
    matched_catheter_id <- vapply(all_infections, function(inf) {
      match_active_catheter_id(inf$patient_id, inf$infection_date)
    }, character(1))
    keep <- !is.na(matched_catheter_id)
    split(all_infections[keep], matched_catheter_id[keep])
  } else {
    list()
  }

  # Build pd_catheter objects, each owning its episodes
  build_patient_catheters <- function(pid) {
    df <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
    out <- list()
    for (i in seq_len(nrow(df))) {
      cid <- df$catheter_id[i]
      cath_infections <- infections_by_catheter[[cid]]
      if (is.null(cath_infections)) cath_infections <- list()

      cath <- tryCatch(
        pd_catheter(
          patient_id = df$patient_id[i],
          catheter_id = cid,
          insertion_date = df$insertion_date[i],
          procedure_type = as.character(df$procedure_type[i]),
          pd_start_date = df$pd_start_date[i],
          pd_stop_date = df$pd_stop_date[i],
          removal_reason = as.character(df$removal_reason[i]),
          infections = cath_infections,
          # t0/t1 come from pd_unit() so n_peritonitis_episodes and peritonitis_flag are scoped to the survey period
          t0 = t0,
          t1 = t1
        ),
        error = function(e) {
          log$add("Catheter ", cid, ": ", conditionMessage(e),
                  " (catheter skipped)")
          NULL
        }
      )
      if (!is.null(cath)) out[[length(out) + 1]] <- cath
    }
    out
  }

  # Build pd_patient objects, each owning its catheters
  patient_list <- list()
  # transfer_details <- character(0)

  for (pid in pids) {
    tau <- taus[[pid]]

    # PD cohort at any point during [t0, t1], censored at tau
    cath_rows <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
    if (!on_pd_in_period(cath_rows, t0, t1, tau$date)) {
      next
    }

    catheters <- build_patient_catheters(pid)

    p <- tryCatch(
      pd_patient(
        patient_id = pid,
        catheters = catheters,
        t0 = t0,
        t1 = t1,
        transfer_reason = tau$reason,
        transfer_date = tau$date
      ),
      error = function(e) {
        log$add("Patient ", pid, ": ", conditionMessage(e),
                " (patient skipped)")
        NULL
      }
    )
    if (!is.null(p)) {
      patient_list[[length(patient_list) + 1]] <- p
      # transfer_details[pid] <- if (is.null(tau$detail)) NA_character_ else tau$detail
    }
  }

  # Flatten the object graph into the unit's three tibbles
  patients_tbl <- patients_to_tibble(patient_list, t0, t1, details)
  catheters_tbl <- catheters_to_tibble(patient_list, t0, t1)
  infections_tbl <- infections_to_tibble(patient_list, t0, t1)

  # ---- 10. unit-level numbers ---------------------------------------------
  # Cohort size: everyone on PD at any point in [t0, t1].
  n_patients <- length(patient_list)

  # Incident ("new") patients: earliest PD start inside [t0, t1]. Each
  # pd_patient already derived this as new_patient_flag via
  # is_incident_patient(), so summing the flags keeps one definition of
  # "incident" rather than recomputing it from the raw table. A patient with
  # no usable start dates has flag NA and is not counted.
  n_new <- sum(vapply(patient_list, function(p) isTRUE(p$new_patient_flag),
                      logical(1)))

  # PY: total patient-years at risk, censored to [t0, t1] and each patient's tau
  tpyar <- total_patient_years(patient_list, t0, t1)

  report_issues(log, strict = strict)

  x <- new_pd_unit(
    unit_id      = unit_id,
    t0           = t0,
    t1           = t1,
    n_new        = as.integer(n_new),
    n_patients   = as.integer(n_patients),
    tpyar        = tpyar,
    patients     = patients_tbl,
    catheters    = catheters_tbl,
    infections   = infections_tbl,
    patient_list = patient_list
  )

  validate_pd_unit(x)
}


# These are the helper functions to ingest raw files.
# pd_unit() calls them in order: create_catheter_id() -> build_infections_by_catheter() -> build_patient_list()


#' Create catheter_id from patient_id and insertion_date
#'
#' This builds \code{catheter_id} as \code{patient_id} plus a two-digit sequence number
#' ordered by \code{insertion_date} within each patient (e.g. a patient
#' \code{"XYZ1234"}'s first catheter becomes \code{"XYZ1234_01"}, and so on).
#' @noRd
create_catheter_id <- function(patient_id, insertion_date) {
  stopifnot(length(patient_id) == length(insertion_date))
  stopifnot(inherits(insertion_date, "Date"))  # order by date, not string

  catheter_id <- rep(NA_character_, length(patient_id))  # NA patient stays NA

  for (pid in unique(patient_id[!is.na(patient_id)])) {
    rows <- which(patient_id == pid)  # this patient's rows
    # NAs sort last, so a catheter with no insertion_date gets the highest sequence number rather than silently displacing a dated one
    ord <- rows[order(insertion_date[rows], na.last = TRUE)]
    catheter_id[ord] <- paste0(pid, "_", sprintf("%02d", seq_along(ord)))  # _01, _02, ...
  }
  catheter_id
}




#' Match one episode to the catheter that was active on its infection_date
#'
#' @param raw_catheters Data frame of every catheter row, with \code{patient_id},
#'   \code{catheter_id}, \code{pd_start_date} and \code{pd_stop_date}.
#' @noRd
match_active_catheter_id <- function(pid, infection_date, raw_catheters, log) {
  cath <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]  # this patient's catheters
  if (nrow(cath) == 0) {
    return(NA_character_)  # no catheters on file
  }
  # ACTIVE = started on/before infection and not yet stopped (NA stop = still in use)
  active <- !is.na(cath$pd_start_date) &
    cath$pd_start_date <= infection_date &
    (is.na(cath$pd_stop_date) | infection_date <= cath$pd_stop_date)
  candidates <- cath$catheter_id[active]

  if (length(candidates) == 0) {
    # Log and return NA so the episode is skipped by later checks
    log$add("Patient ", pid, ": no active PD catheter on infection_date ",
            format(infection_date), "; this episode is not attached to a catheter and is not ",
            "checked further. Check the episode date and this patient's catheter dates in the source files.")
    return(NA_character_)
  }
  if (length(candidates) > 1) {
    # Overlapping windows are a data error: log it, then fall back to the first match
    log$add("Patient ", pid, ": ", length(candidates),
            " overlapping PD catheters active on infection_date ",
            format(infection_date), " (",
            paste(candidates, collapse = ", "),
            "); earliest used. Correct the insertion_date/pd_start_date/pd_stop_date of these catheters so their windows don't overlap.")
  }
  candidates[1]  # first match (the earliest, if raw_catheters is sorted by date)
  # cath$catheter_id[active][which.min(cath$pd_start_date[active])]     # use this if not sorted by date
}




#' Build one patient's pd_infection objects, each with its episode type classified
#'
#' The patient's episodes are classified in one call to
#' \code{classify_episode_types()}, which compares each episode with the
#' patient's preceding valid episode (timing of the last antibiotic dose and
#' organism) using the compiled C++ loop unless \code{use_cpp = FALSE}. Each
#' episode is then built and validated. An episode that fails validation is
#' logged and skipped; as in \code{classify_episode_types()}, it is not used as
#' the prior episode for the next one.
#'
#' @param df Data frame of one patient's PE rows, ordered by
#'   \code{date_of_infection}, with \code{organism_list}, \code{outcome} and
#'   \code{outcome_date} already derived.
#' @param log Issue log from \code{new_issue_log()}.
#' @param use_cpp Logical. Passed to \code{classify_episode_types()}.
#'   \code{TRUE} by default, which is what \code{pd_unit()} uses.
#'
#' @return A list of \code{pd_infection} objects.
#' @noRd
#'
build_patient_infections <- function(df, log, use_cpp = TRUE) {
  if (nrow(df) == 0) {
    return(list())
  }

  # Rows that fail validation come back NA and are logged one by one below, so the
  # classifier's own warning about them is silenced here to avoid reporting twice.
  types <- suppressWarnings(classify_episode_types(
    patient_id = df$patient_id,
    infection_date = df$date_of_infection,
    last_dose_antibiotic = df$last_dose_antibiotic,
    organism_list = df$organism_list,
    outcome = df$outcome,
    outcome_date = df$outcome_date,
    use_cpp = use_cpp
  ))

  infections <- list()
  for (i in seq_len(nrow(df))) {
    inf <- tryCatch(
      validate_pd_infection(new_pd_infection(
        patient_id = df$patient_id[i],
        infection_date = df$date_of_infection[i],
        organism_list = df$organism_list[[i]],  # list column, so [[i]]
        episode_type = types[i],
        last_dose_antibiotic = df$last_dose_antibiotic[i],
        outcome = df$outcome[i],
        outcome_date = df$outcome_date[i]
      )),
      error = function(e) {
        # Validation failure; log and skip
        log$add("Patient ", df$patient_id[i], ", episode on ",
                format(df$date_of_infection[i]), ": ", conditionMessage(e),
                " (episode not checked further; correct the source file)")
        NULL
      }
    )
    if (!is.null(inf)) {
      infections[[length(infections) + 1]] <- inf  # append
    }
  }
  infections
}




#' Build every pd_infection object and group them by the catheter that owns them
#'
#' @param raw_pe_episodes Data frame of PE rows (from \code{build_patient_infections()}).
#' @param raw_catheters Data frame of every catheter row.
#' @param log Issue log from \code{new_issue_log()}.
#'
#' @return A named list of \code{pd_infection} lists, one element per
#'   \code{catheter_id} that owns at least one episode. Episodes that match no
#'   catheter are logged and left out.
#' @noRd
#'
build_infections_by_catheter <- function(raw_pe_episodes, raw_catheters, log) {
  # the split() func silently drops NA patient_ids, so log and remove them explicitly
  missing_pid <- is.na(raw_pe_episodes$patient_id)
  if (any(missing_pid)) {
    log$add(sum(missing_pid), " PE episode(s) have a missing patient_id",
            "; not attached to a catheter and not checked further.",
            " Fill in patient_id in the source file.")
    raw_pe_episodes <- raw_pe_episodes[!missing_pid, , drop=FALSE]
  }

  # build each patient's episodes separately so the prior-episode comparison stays within a patient
  infections_by_patient <- if (nrow(raw_pe_episodes) > 0) {
    lapply(split(raw_pe_episodes, raw_pe_episodes$patient_id),
           build_patient_infections, log = log, use_cpp = TRUE)
  } else {
    list()
  }
  # flatten to one list of pd_infection objects
  all_infections <- unlist(infections_by_patient, recursive = FALSE, use.names = FALSE)
  if (is.null(all_infections)) all_infections <- list()

  if (length(all_infections) == 0) {
    return(list())
  }

  matched_catheter_id <- vapply(all_infections, function(inf) {
    match_active_catheter_id(inf$patient_id, inf$infection_date,
                             raw_catheters, log)           # one catheter_id (or NA) per episode
  }, character(1))
  keep <- !is.na(matched_catheter_id)                      # drop unmatched episodes (already logged)
  split(all_infections[keep], matched_catheter_id[keep])   # group by owning catheter
}




#' Build one patient's pd_catheter objects, each owning its peritonitis episodes
#' @noRd
build_patient_catheters <- function(pid, raw_catheters, infections_by_catheter,
                                    t0, t1, log, tau = as.Date(NA)) {
  df <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
  out <- list()
  for (i in seq_len(nrow(df))) {
    cid <- df$catheter_id[i]
    cath_infections <- infections_by_catheter[[cid]]           # episodes matched to this catheter
    if (is.null(cath_infections)) cath_infections <- list()    # none matched

    # days at risk in [t0, t1] using catheter_exposure_days()
    exposure <- as.integer(catheter_exposure_days(
      list(pd_start_date = df$pd_start_date[i], pd_stop_date = df$pd_stop_date[i]),
      t0, t1, tau = tau
    ))

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
        t0 = t0,
        t1 = t1,
        total_exposure_days = exposure
      ),
      error = function(e) {
        # a catheter that fails validation is logged and left out of the remaining checks
        log$add("Catheter ", cid, ": ", conditionMessage(e),
                " (catheter not checked further; correct the source file)")
      }
    )
    if (!is.null(cath)) out[[length(out) + 1]] <- cath        # keep valid catheters only
  }
  out
}




#' Build the pd_patient objects for everyone on PD during the reporting period
#'
#' A patient is in the cohort if they were on PD at any point in
#' \code{[t0, t1]}, censored at their \code{tau}. Each pd_patient owns its
#' catheters. A patient whose every catheter failed validation is logged,
#' because \code{pd_patient()} would accept the empty catheter list and leave
#' them counted in \code{n_patients} with zero patient-years.
#'
#' @param pids Character vector of patient ids to consider.
#' @param taus Named list (by patient id) from \code{derive_patient_tau()}.
#' @param raw_catheters,raw_patients Data frames of catheter and patient rows.
#' @param infections_by_catheter Named list from \code{build_infections_by_catheter()}.
#' @param t0,t1 Dates. Reporting period.
#' @param log Issue log from \code{new_issue_log()}.
#'
#' @return A list with \code{patient_list} (list of \code{pd_patient}),
#'   \code{transfer_details} (character, named by patient id; the detail of each
#'   patient's censoring event).
#' @noRd
#'
build_patient_list <- function(pids, taus, raw_catheters, raw_patients,
                               infections_by_catheter, t0, t1, log) {
  patient_list <- list()
  transfer_details <- character(0)

  for (pid in pids) {
    tau <- taus[[pid]]    # censoring date/reason/detail for this patient

    cath_rows <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
    if (!on_pd_in_period(cath_rows, t0, t1, tau$date)) {
      next      # not on PD in [t0, t1]
    }

    catheters <- build_patient_catheters(pid, raw_catheters,
                                         infections_by_catheter, t0, t1, log,
                                         tau = tau$date)

    # pd_patient() accepts an empty list and would count them with zero patient-years, show warning
    if (length(catheters) == 0) {
      log$add("Patient ", pid, ": no valid PD catheter remains after ",
              "validation (see the catheter issue(s) above); the patient ",
              "cannot be included in the unit's counts or patient-years.")
    }

    demo <- raw_patients[raw_patients$patient_id == pid, , drop = FALSE]  # this patient's A3 row

    p <- tryCatch(
      pd_patient(
        patient_id = pid,
        catheters = catheters,
        t0 = t0,
        t1 = t1,
        gender = patient_demo_value(demo, "gender"),
        ethnicity = patient_demo_value(demo, "ethnicity"),
        date_of_birth = patient_dob_value(demo),
        primary_kidney_disease = patient_demo_value(demo, "primary_kidney_disease"),
        diabetes_status = patient_demo_value(demo, "diabetes_type"),
        smoking_status = patient_demo_value(demo, "cigarette_smoking_status"),
        dialysis_type = patient_demo_value(demo, "dialysis_type"),
        transfer_reason = tau$reason,
        transfer_date = tau$date
      ),
      error = function(e) {
        # validation failure: log and skip this patient
        log$add("Patient ", pid, ": ", conditionMessage(e),
                " (patient not checked further; correct the source file)")
        NULL
      }
    )
    if (!is.null(p)) {
      patient_list[[length(patient_list) + 1]] <- p
      # keep the censoring detail aligned with patient_list, named by patient id
      transfer_details[pid] <- if (is.null(tau$detail)) NA_character_ else tau$detail
    }
  }
  list(patient_list = patient_list,
       transfer_details = transfer_details)
}




#' Read one demographic value for a patient off their A3 form row
#' @noRd
patient_demo_value <- function(demo, col) {
  if (nrow(demo) == 0 || !(col %in% names(demo))) {        # no row or no such column = NA instead of an error
    return(NA_character_)
  }
  val <- as.character(demo[[col]][1])                      # first row only
  if (is.na(val) || !nzchar(trimws(val))) NA_character_ else trimws(val)     # blank/whitespace becomes NA, otherwise trimmed
}



#' Read this patient's date of birth off their A3 form row
#' @noRd
patient_dob_value <- function(demo) {
  if (nrow(demo) == 0 || !("date_of_birth" %in% names(demo))) {        # no row or no such column = typed Date NA, not character NA
    return(as.Date(NA))
  }
  val <- demo$date_of_birth[1]
  if (is.na(val)) as.Date(NA) else val  # wrap NA so the class is always Date
}



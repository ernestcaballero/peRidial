
# These are helper functions to ingest raw files, specifically to build patient,
# catheter, and infection objects


#' Create catheter_id from patient_id and insertion_date
#'
#' A patient can have more than one PD catheter over time. This builds
#' \code{catheter_id} as \code{patient_id} plus a two-digit sequence number
#' ordered by \code{insertion_date} within each patient (e.g. a patient
#' \code{"XYZ1234"}'s first catheter becomes \code{"XYZ1234_01"}, and so on).
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
    # NAs sort last, so a catheter with no insertion_date gets the highest sequence number rather than silently displacing a dated one
    ord <- rows[order(insertion_date[rows], na.last = TRUE)]
    catheter_id[ord] <- paste0(pid, "_", sprintf("%02d", seq_along(ord)))
  }

  catheter_id
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




# Build pd_infection objects, chained per patient
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



# Build pd_catheter objects, each owning its peritonitis episodes if any
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



# Build pd_patient objects, each owning its catheters if any
patient_list <- list()
transfer_details <- character(0)
no_catheter_pids <- character(0)  # in-cohort patients left with no valid catheter

for (pid in pids) {
  tau <- taus[[pid]]

  # PD cohort at any point during [t0, t1], censored at tau
  cath_rows <- raw_catheters[raw_catheters$patient_id == pid, , drop = FALSE]
  if (!on_pd_in_period(cath_rows, t0, t1, tau$date)) {
    next
  }

  catheters <- build_patient_catheters(pid)

  # every catheter failed validation (each already logged above). pd_patient()
  # would accept an empty catheter list, leaving this patient counted in
  # n_patients with zero patient-years, so flag it: report_issues() below
  # raises it as an error rather than a warning
  if (length(catheters) == 0) {
    no_catheter_pids <- c(no_catheter_pids, pid)
    log$add("Patient ", pid, ": no valid PD catheter remains after ",
            "validation (see the catheter issue(s) above); the patient ",
            "cannot be included in the unit's counts or patient-years. ",
            "This is always an error, even with strict = FALSE.")
  }

  demo <- raw_patients[raw_patients$patient_id == pid, , drop = FALSE]

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
      log$add("Patient ", pid, ": ", conditionMessage(e),
              " (patient skipped)")
      NULL
    }
  )
  if (!is.null(p)) {
    patient_list[[length(patient_list) + 1]] <- p
    transfer_details[pid] <- if (is.null(tau$detail)) NA_character_ else tau$detail
  }
}





#' Read one demographic value for a patient off their A3 form row
#'
#' Small helper behind the \code{gender}/\code{ethnicity}/
#' \code{primary_kidney_disease}/\code{diabetes_type}/
#' \code{cigarette_smoking_status} lookups in \code{pd_unit()}: guards
#' against a patient with no matching row in \code{raw_patients} (returns
#' \code{NA} rather than erroring) and normalises a blank/\code{NA} cell to
#' \code{NA_character_}.
#'
#' @param demo One-row data frame of this patient's demographic fields (see
#'   \code{raw_patients} in \code{pd_unit()}).
#' @param col Character. Column name to read from \code{demo}.
#'
#' @return A single character value, or \code{NA_character_}.
#' @noRd
#'
patient_demo_value <- function(demo, col) {
  if (nrow(demo) == 0 || !(col %in% names(demo))) {
    return(NA_character_)
  }
  val <- as.character(demo[[col]][1])
  if (is.na(val) || !nzchar(trimws(val))) NA_character_ else trimws(val)
}




#' Read this patient's date of birth off their A3 form row
#'
#' Date-typed counterpart to \code{patient_demo_value()}: guards against a
#' patient with no matching row in \code{raw_patients} (returns
#' \code{as.Date(NA)} rather than erroring), while keeping the \code{Date}
#' class that \code{date_of_birth} needs. \code{patient_demo_value()}
#' always returns character, so it can't be reused for this column.
#'
#' @param demo One-row data frame of this patient's demographic fields (see
#'   \code{raw_patients} in \code{pd_unit()}).
#'
#' @return A single \code{Date} value, or \code{as.Date(NA)}.
#' @noRd
#'
patient_dob_value <- function(demo) {
  if (nrow(demo) == 0 || !("date_of_birth" %in% names(demo))) {
    return(as.Date(NA))
  }
  val <- demo$date_of_birth[1]
  if (is.na(val)) as.Date(NA) else val
}




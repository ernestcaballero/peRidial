

#' Create pd_patient object
#'
#' One person receiving PD at any point during the reporting period. Owns the
#' catheters belonging to that patient, so a patient's whole PD history lives in one object.
#' Each of those catheters in turn owns its own peritonitis episodes, giving the nested structure.
#'
#' @param patient_id Character. The patient's unique identifier (NHI).
#' @param catheters List. A list of \code{pd_catheter} objects belonging to
#'   this patient (i.e. each has \code{patient_id} equal to this patient's
#'   \code{patient_id}). Defaults to an empty list.
#' @param t0 Date. Start of the reporting period, used to derive
#'   \code{new_patient_flag}. Defaults to \code{NA}.
#' @param t1 Date. End of the reporting period. See \code{t0}.
#' @param transfer_reason Character. Reason the patient left PD permanently
#'   (e.g. \code{"death"}, \code{"transplant"}, \code{"permanent transfer to HD"}),
#'   or \code{NA} if they are still on PD. This together with
#'   \code{transfer_date} is the patient-level censoring point \eqn{tau} from
#'   the design proposal.
#' @param transfer_date Date. Date the patient left PD (\eqn{tau}). Required
#'   whenever \code{transfer_reason} is supplied, and vice versa.
#' @param new_patient_flag Logical. \code{TRUE} if this patient first started
#'   PD within \code{[t0, t1]} (incident), \code{FALSE} if they were already on
#'   PD before \code{t0} (prevalent). Defaults to \code{NULL}, which derives it
#'   from \code{catheters}/\code{t0}/\code{t1} via
#'   \code{is_incident_patient()}; if supplied explicitly it is checked for
#'   consistency.
#' @param n_catheters Integer. Number of catheters this patient has. Defaults
#'   to \code{NULL}, which derives it as \code{length(catheters)}; if supplied
#'   explicitly it is checked for consistency.
#' @param n_infections Integer. Count of countable peritonitis episodes across
#'   all of this patient's catheters within the reporting period -- the
#'   \eqn{n_i} of Equation (3). Relapsing episodes are excluded and the count
#'   is scoped to \code{[t0, t1]}, both inherited from each catheter's own
#'   \code{n_peritonitis_episodes}. Defaults to \code{NULL}, which derives it
#'   via \code{count_patient_episodes()}; if supplied explicitly it is checked
#'   for consistency.
#'
#' @returns An object of class \code{pd_patient}.
#' @export
#'
new_pd_patient <- function(patient_id = NA_character_,
                           # demographics = list(),
                           catheters = list(),
                           t0 = as.Date(NA),
                           t1 = as.Date(NA),
                           transfer_reason = NA_character_,
                           transfer_date = as.Date(NA),
                           new_patient_flag = NULL,
                           n_catheters = NULL,
                           n_infections = NULL) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(patient_id))
  # stopifnot(is.list(demographics))
  stopifnot(is.list(catheters))
  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))
  stopifnot(is.character(transfer_reason) || is.na(transfer_reason))
  stopifnot(inherits(transfer_date, "Date"))

  # new_patient_flag / n_catheters / n_infections are derived from `catheters` (and t0/t1)
  if (is.null(new_patient_flag)) {
    new_patient_flag <- is_incident_patient(catheters, t0, t1)
  }
  if (is.null(n_catheters)) {
    n_catheters <- length(catheters)
  }
  if (is.null(n_infections)) {
    n_infections <- count_patient_episodes(catheters)
  }

  stopifnot(length(new_patient_flag) == 1,
            is.na(new_patient_flag) || is.logical(new_patient_flag))
  stopifnot(length(n_catheters) == 1,
            is.na(n_catheters) || is.numeric(n_catheters))
  stopifnot(length(n_infections) == 1,
            is.na(n_infections) || is.numeric(n_infections))

  structure(
    list(
      patient_id = patient_id,
      # demographics = demographics,
      catheters = catheters,
      t0 = t0,
      t1 = t1,
      transfer_reason = transfer_reason,
      transfer_date = transfer_date,
      new_patient_flag = new_patient_flag,
      n_catheters = n_catheters,
      n_infections = n_infections
    ),
    class = "pd_patient"
  )
}


validate_pd_patient <- function(x) {
  stopifnot(inherits(x, "pd_patient"))

  if (is.na(x$patient_id)) {
    stop("patient_id must be supplied.")
  }
  # reporting period, if known, must be ordered sensibly
  if (!is.na(x$t0) && !is.na(x$t1) && x$t0 > x$t1) {
    stop("t0 must be on or before t1.")
  }
  # No transfer_reason (implied still on PD), but transfer_date supplied -- invalid
  if (is.na(x$transfer_reason) && !is.na(x$transfer_date)) {
    stop("transfer_date must be matched with a transfer_reason.")
  }
  # transfer_reason supplied but no date to go with it
  if (!is.na(x$transfer_reason) && is.na(x$transfer_date)) {
    stop("Missing date of transfer. Must be supplied.")
  }

  # Nested pd_catheter checks
  if (length(x$catheters) > 0) {
    for (i in seq_along(x$catheters)) {
      catheter <- x$catheters[[i]]

      if (!inherits(catheter, "pd_catheter")) {
        stop("catheters[[", i, "]] is not a pd_catheter object.")
      }
      if (!identical(catheter$patient_id, x$patient_id)) {
        stop("catheters[[", i, "]] has patient_id '", catheter$patient_id,
             "', which does not match this patient's patient_id '",
             x$patient_id, "'.")
      }
      # a patient's PD can't continue past their censoring date (tau): death, transplant, or permanent transfer to HD
      if (!is.na(x$transfer_date) && !is.na(catheter$pd_stop_date) &&
          catheter$pd_stop_date > x$transfer_date) {
        stop("catheters[[", i, "]] has pd_stop_date after this patient's ",
             "transfer_date (tau).")
      }
    }

    # catheter_ids must be unique within a patient
    cath_ids <- vapply(x$catheters, function(cath) cath$catheter_id, character(1))
    if (anyDuplicated(cath_ids) > 0) {
      stop("Duplicate catheter_id(s) within this patient: ",
           paste(unique(cath_ids[duplicated(cath_ids)]), collapse = ", "), ".")
    }

    # Non-overlapping catheter intervals. Two catheters may not deliver PD
    # to the same patient at the same time, so sorting by start date and
    # checking each interval begins after the previous one ended is enough.
    # An open interval (pd_stop_date NA) is treated as running to the end of time, so nothing may start after it.
    starts <- do.call(c, lapply(x$catheters, function(cath) cath$pd_start_date))
    stops  <- do.call(c, lapply(x$catheters, function(cath) cath$pd_stop_date))

    if (!anyNA(starts) && length(starts) > 1) {
      ord <- order(starts)
      starts <- starts[ord]
      stops  <- stops[ord]
      ids    <- cath_ids[ord]

      for (i in seq_len(length(starts) - 1)) {
        # an open-ended catheter can't be followed by another one
        if (is.na(stops[i])) {
          stop("Catheter '", ids[i], "' has no pd_stop_date (still active) ",
               "but catheter '", ids[i + 1], "' starts after it -- ",
               "catheter intervals within a patient must not overlap.")
        }
        if (starts[i + 1] < stops[i]) {
          stop("Catheters '", ids[i], "' and '", ids[i + 1], "' have ",
               "overlapping PD intervals -- catheter intervals within a ",
               "patient must not overlap.")
        }
      }
    }
  }

  # new_patient_flag / n_catheters / n_infections must stay consistent with catheters (and t0/t1)
  expected_flag <- is_incident_patient(x$catheters, x$t0, x$t1)
  if (!is.na(x$new_patient_flag) && !is.na(expected_flag) &&
      !identical(x$new_patient_flag, expected_flag)) {
    stop("new_patient_flag does not match whether this patient's earliest ",
         "pd_start_date falls within [t0, t1].")
  }
  if (!is.na(x$n_catheters) && x$n_catheters != length(x$catheters)) {
    stop("n_catheters does not match length(catheters).")
  }
  expected_n <- count_patient_episodes(x$catheters)
  if (!is.na(x$n_infections) && x$n_infections != expected_n) {
    stop("n_infections does not match the total peritonitis episodes across ",
         "this patient's catheters within [t0, t1].")
  }

  x
}




#' Determine whether a patient is incident (new to PD) in a reporting period
#'
#' A patient is "incident" (a new PD patient) for the reporting period \code{[t0, t1]}
#' if the \emph{earliest} PD start date falls inside that window.
#'
#' Either t0/t1 boundary can be \code{NA}, in which case that side is left unfiltered.
#' If both are \code{NA} there is no window to be incident \emph{within}, so this returns \code{NA}.
#'
#' @param catheters List of \code{pd_catheter} objects for one patient.
#' @param t0 Date. Start of the reporting period, or \code{NA}.
#' @param t1 Date. End of the reporting period, or \code{NA}.
#'
#' @return \code{TRUE} if the patient first started PD within \code{[t0, t1]},
#'   \code{FALSE} if they started before it, or \code{NA} when there is nothing
#'   to decide from (no catheters, no usable start dates, or
#'   no reporting window supplied).
#' @noRd
#'
is_incident_patient <- function(catheters,
                                t0 = as.Date(NA),
                                t1 = as.Date(NA)) {
  # no window supplied -> "incident within what?" is undecidable
  if (is.na(t0) && is.na(t1)) {
    return(NA)
  }
  if (length(catheters) == 0) {
    return(NA)
  }

  # Identify start date
  starts <- lapply(catheters, function(cath) {
    # Checks if not a pd_catheter object or pd_start_date is NA
    if (!inherits(cath, "pd_catheter")) {
      return(NULL)
    }
    if (is.na(cath$pd_start_date)) {
      return(NULL)
    }
    cath$pd_start_date
  })
  # Keep the date/s only in a list
  starts <- do.call(c, starts[!vapply(starts, is.null, logical(1))])

  # NA for when all catheters were invalid or had no start date and NULL-from-empty-list
  if (length(starts) == 0) {
    return(NA)
  }

  # earliest PD start across every valid pd_start_date this patient has
  first_start <- min(starts)

  in_window <- TRUE
  if (!is.na(t0)) in_window <- in_window && first_start >= t0
  if (!is.na(t1)) in_window <- in_window && first_start <= t1

  in_window
}



#' Count peritonitis episodes across all of patient's catheters
#'
#' Sums \code{n_peritonitis_episodes} over every \code{pd_catheter} in \code{catheters}.
#'
#' @param catheters List of \code{pd_catheter} objects for one patient.
#'
#' @return A single non-negative integer count.
#' @noRd
#'
count_patient_episodes <- function(catheters) {
  if (length(catheters) == 0) {
    return(0L)
  }

  counts <- vapply(catheters, function(cath) {
    if (!inherits(cath, "pd_catheter")) {
      return(0)
    }
    n <- cath$n_peritonitis_episodes
    if (is.null(n) || is.na(n)) {
      return(0)
    }
    as.numeric(n)
  }, numeric(1))

  sum(counts)
}





#' Construct and validate a pd_patient object
#'
#' Builds a \code{pd_patient} via \code{new_pd_patient()} and checks it with
#' \code{validate_pd_patient()} before returning it.
#'
#' @inheritParams new_pd_patient
#'
#' @return An object of class \code{pd_patient}.
#' @export
#'
pd_patient <- function(patient_id,
                       # demographics = list(),
                       catheters = list(),
                       t0 = as.Date(NA),
                       t1 = as.Date(NA),
                       transfer_reason = NA_character_,
                       transfer_date = as.Date(NA),
                       new_patient_flag = NULL,
                       n_catheters = NULL,
                       n_infections = NULL) {
  x <- new_pd_patient(patient_id = patient_id,
                      # demographics = demographics,
                      catheters = catheters,
                      t0 = t0,
                      t1 = t1,
                      transfer_reason = transfer_reason,
                      transfer_date = transfer_date,
                      new_patient_flag = new_patient_flag,
                      n_catheters = n_catheters,
                      n_infections = n_infections
                      )

  validate_pd_patient(x)
}

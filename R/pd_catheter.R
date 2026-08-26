


#' Create pd_catheter object
#'
#' A single PD catheter episode for a patient: when it was inserted,
#' the window of active PD therapy, and (if applicable) why and
#' when it was removed. Owns the peritonitis episodes ("infections") that
#' occurred during this catheter's active PD window, so episode counts and
#' their matching denominator live in the same object.
#'
#' @param patient_id Character. The patient's unique identifier (NHI).
#' @param catheter_id Character. Unique identifier for this catheter.
#' @param insertion_date Date. Date the catheter was surgically inserted.
#' @param procedure_type Character. Type of insertion technique (eg. \code{"open surgical"},
#'    \code{"laparoscopic"}, \code{"percutaneous"}).
#' @param pd_start_date Date. Date of start of PD therapy of this catheter,
#'    usually 2-4 weeks after insertion.
#' @param pd_stop_date Date. Date PD therapy stopped on this catheter, or
#'   \code{NA} if the catheter is still in active use.
#' @param removal_reason Character. Reason the catheter was removed (e.g. \code{"infection"},
#'   \code{"mechanical failure"}, \code{"transplant"}), or \code{NA} if it hasn't been removed.
#' @param infections List. A list of \code{pd_infection} objects that occurred
#'   on this catheter (i.e. each has \code{catheter_id} equal to this
#'   catheter's \code{catheter_id}). Defaults to an empty list for a catheter
#'   with no recorded peritonitis episodes. This is restricted to the reporting
#'   period itself (\code{t0} and \code{t1}).
#' @param t0 Date. Start of the reporting period, used only to scope
#'   \code{n_peritonitis_episodes}/\code{peritonitis_flag} to episodes that
#'   fall inside \code{[t0, t1]}. Defaults to \code{NA}, which counts every
#'   episode in \code{infections} with no period filtering.
#' @param t1 Date. End of the reporting period. See \code{t0}.
#' @param total_exposure_days Integer. Total days this catheter is at risk,
#'   i.e. \code{exposure_days} from Equation (1) of the design proposal
#'   (censored against \code{t0}, \code{t1}, and the patient's censoring date
#'   \code{tau} for death, transplant, or permanent HD transfer). Patient-level
#'   \code{tau} isn't known to a \code{pd_catheter} object in isolation, so
#'   this is computed upstream (at the \code{pd_unit}/ingest level) and
#'   supplied here; defaults to \code{NA_integer_} until that computation is
#'   wired in.
#' @param n_peritonitis_episodes Integer. Count of peritonitis episodes that
#'   occurred on this catheter \emph{within the reporting period} \code{[t0, t1]}
#'   -- 0, 1, 2, etc. Relapsing episodes are excluded from this count (see
#'   \code{count_episodes_in_period()}); recurrent/repeat episodes are
#'   included.
#' @param peritonitis_flag Logical. \code{TRUE} if
#'   \code{n_peritonitis_episodes > 0} for this catheter within the reporting
#'   period, \code{FALSE} otherwise.
#'
#' @returns An object of class \code{pd_catheter}.
#' @export
#'
new_pd_catheter <- function(patient_id = NA_character_,
                            catheter_id = NA_character_,
                            insertion_date = as.Date(NA),
                            procedure_type = NA_character_,
                            pd_start_date = as.Date(NA),
                            pd_stop_date = as.Date(NA),
                            removal_reason = NA_character_,
                            infections = list(),
                            t0 = as.Date(NA),
                            t1 = as.Date(NA),
                            total_exposure_days = NA_integer_,
                            n_peritonitis_episodes = NULL,
                            peritonitis_flag = NULL) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(patient_id))
  stopifnot(length(catheter_id) == 1, is.character(catheter_id) || is.na(catheter_id))
  stopifnot(inherits(insertion_date, "Date"))
  stopifnot(is.character(procedure_type) || is.na(procedure_type))
  stopifnot(inherits(pd_start_date, "Date"))
  stopifnot(inherits(pd_stop_date, "Date"))
  stopifnot(is.character(removal_reason) || is.na(removal_reason))
  stopifnot(is.list(infections))
  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))
  stopifnot(length(total_exposure_days) == 1,
            is.na(total_exposure_days) || is.numeric(total_exposure_days))

  # n_peritonitis_episodes / peritonitis_flag count episodes within the survey period [t0, t1]
  if (is.null(n_peritonitis_episodes)) {
    n_peritonitis_episodes <- count_episodes_in_period(infections, t0, t1)
  }
  if (is.null(peritonitis_flag)) {
    peritonitis_flag <- n_peritonitis_episodes > 0
  }

  stopifnot(length(n_peritonitis_episodes) == 1,
            is.na(n_peritonitis_episodes) || is.numeric(n_peritonitis_episodes))
  stopifnot(length(peritonitis_flag) == 1,
            is.na(peritonitis_flag) || is.logical(peritonitis_flag))

  structure(
    list(
      patient_id = patient_id,
      catheter_id = catheter_id,
      insertion_date = insertion_date,
      procedure_type = procedure_type,
      pd_start_date = pd_start_date,
      pd_stop_date = pd_stop_date,
      removal_reason = removal_reason,
      infections = infections,
      t0 = t0,
      t1 = t1,
      total_exposure_days = total_exposure_days,
      n_peritonitis_episodes = n_peritonitis_episodes,
      peritonitis_flag = peritonitis_flag
    ),
    class = "pd_catheter"
  )
}


validate_pd_catheter <- function(x) {
  stopifnot(inherits(x, "pd_catheter"))

  if (is.na(x$patient_id) || is.na(x$catheter_id)) {
    stop("Both patient_id and catheter_id must be supplied.")
  }
  if (is.na(x$insertion_date)) {
    stop("Missing insertion_date. Must be supplied.")
  }
  if (is.na(x$pd_start_date)) {
    stop("Missing pd_start_date. Must be supplied.")
  }
  # a catheter can't start PD therapy before it's actually been inserted
  if (x$insertion_date > x$pd_start_date) {
    stop("insertion_date must be on or before pd_start_date.")
  }
  # if PD therapy has stopped, the start/stop window must make sense
  if (!is.na(x$pd_stop_date) && x$pd_start_date > x$pd_stop_date) {
    stop("pd_start_date must be on or before pd_stop_date.")
  }
  # a removal_reason means the catheter has actually stopped being used
  if (!is.na(x$removal_reason) && is.na(x$pd_stop_date)) {
    stop("pd_stop_date must be supplied when removal_reason is given.")
  }
  # reporting period, if known, must be ordered sensibly
  if (!is.na(x$t0) && !is.na(x$t1) && x$t0 > x$t1) {
    stop("t0 must be on or before t1.")
  }
  # total_exposure_days, if known, can't be negative, and can't exceed the
  # raw exposure days the catheter was actually open for (further censored against t0/t1/tau)
  if (!is.na(x$total_exposure_days)) {
    if (x$total_exposure_days < 0) {
      stop("total_exposure_days cannot be negative.")
    }
    if (!is.na(x$pd_stop_date)) {
      raw_exposure_days <- as.numeric(x$pd_stop_date - x$pd_start_date)
      if (x$total_exposure_days > raw_exposure_days) {
        stop("total_exposure_days cannot exceed the span between ",
             "pd_start_date and pd_stop_date.")
      }
    }
  }

  # Nested infection checks
  if (length(x$infections) > 0) {
    for (i in seq_along(x$infections)) {
      infection <- x$infections[[i]]

      if (!inherits(infection, "pd_infection")) {
        stop("infections[[", i, "]] is not a pd_infection object.")
      }
      if (!identical(infection$catheter_id, x$catheter_id)) {
        stop("infections[[", i, "]] has catheter_id '", infection$catheter_id,
             "', which does not match this catheter's catheter_id '",
             x$catheter_id, "'.")
      }
      if (!identical(infection$patient_id, x$patient_id)) {
        stop("infections[[", i, "]] has patient_id '", infection$patient_id,
             "', which does not match this catheter's patient_id '",
             x$patient_id, "'.")
      }
      # infection_date is required before the window checks below
      if (is.na(infection$infection_date)) {
        stop("infections[[", i, "]] has a missing infection_date. ",
             "Must be supplied.")
      }
      # ISPD time-at-risk begins on pd_start_date, so an episode can't be before this date,
      # If the catheter has stopped, an episode can't be after pd_stop_date either.
      if (infection$infection_date < x$pd_start_date) {
        stop("infections[[", i, "]] has infection_date before this ",
             "catheter's pd_start_date.")
      }
      if (!is.na(x$pd_stop_date) && infection$infection_date > x$pd_stop_date) {
        stop("infections[[", i, "]] has infection_date after this ",
             "catheter's pd_stop_date.")
      }
    }
  }

  # n_peritonitis_episodes / peritonitis_flag must stay consistent with infections *within [t0, t1]*
  expected_n <- count_episodes_in_period(x$infections, x$t0, x$t1)
  if (!is.na(x$n_peritonitis_episodes) && x$n_peritonitis_episodes != expected_n) {
    stop("n_peritonitis_episodes does not match the number of infections ",
         "falling within [t0, t1].")
  }
  if (!is.na(x$peritonitis_flag) &&
      !identical(x$peritonitis_flag, expected_n > 0)) {
    stop("peritonitis_flag does not match whether any infection falls ",
         "within [t0, t1].")
  }

  x
}



#' Count peritonitis episodes falling inside a reporting period
#'
#' Counts how many \code{pd_infection} objects in
#' \code{infections} both (a) have an \code{infection_date} inside
#' \code{[t0, t1]}, and (b) are not a \strong{relapsing} episode. Per ISPD, a
#' relapsing episode (same organism, occurring within 4 weeks of completing
#' antibiotics for the immediately preceding episode -- see
#' \code{get_episode_type()} in pd_infection.R) is a continuation of that
#' prior episode rather than a distinct new one, so it is excluded from the
#' count used for rate/free-percentage calculations. \code{"recurrent"} and
#' \code{"repeat"} episodes ARE distinct new episodes and stay counted, as
#' does an episode with \code{NA} \code{episode_type} (e.g. a patient's
#' first-ever recorded episode, with nothing prior to compare against).
#'
#' Either t0/t1 boundary can be \code{NA}, in which case that side is left
#' unfiltered. If both are \code{NA} this only filters out relapses.
#'
#' @param infections List of \code{pd_infection} objects.
#' @param t0 Date. Start of the reporting period, or \code{NA}.
#' @param t1 Date. End of the reporting period, or \code{NA}.
#'
#' @return A single non-negative integer count.
#' @noRd
#'
count_episodes_in_period <- function(infections,
                                     t0 = as.Date(NA),
                                     t1 = as.Date(NA)) {
  if (length(infections) == 0) {
    return(0L)
  }

  counts <- vapply(infections, function(inf) {
    if (!inherits(inf, "pd_infection")) {
      return(FALSE)
    }
    # Infection counter (within the reporting window)
    in_window <- TRUE

    # Checks if episode is on or after t0, on or before t1
    if (!is.na(t0)) in_window <- in_window && inf$infection_date >= t0
    if (!is.na(t1)) in_window <- in_window && inf$infection_date <= t1

    # ISPD: a relapsing episode is a continuation of the preceding episode,
    # not a new one, so exclude it from the count. Recurrent/repeat/NA episodes are counted.
    not_relapse <- is.na(inf$episode_type) || inf$episode_type != "relapsing"

    in_window && not_relapse
  }, logical(1))

  sum(counts)
}




#' Construct and validate a pd_catheter object
#'
#' Builds a \code{pd_catheter} via \code{new_pd_catheter()} and checks it with
#' \code{validate_pd_catheter()} before returning it.
#'
#' @inheritParams new_pd_catheter
#'
#' @return An object of class \code{pd_catheter}.
#' @export
#'
pd_catheter <- function(patient_id,
                        catheter_id,
                        insertion_date,
                        procedure_type = NA_character_,
                        pd_start_date,
                        pd_stop_date = as.Date(NA),
                        removal_reason = NA_character_,
                        infections = list(),
                        t0 = as.Date(NA),
                        t1 = as.Date(NA),
                        total_exposure_days = NA_integer_,
                        n_peritonitis_episodes = NULL,
                        peritonitis_flag = NULL) {
  x <- new_pd_catheter(patient_id = patient_id,
                       catheter_id = catheter_id,
                       insertion_date = insertion_date,
                       procedure_type = procedure_type,
                       pd_start_date = pd_start_date,
                       pd_stop_date = pd_stop_date,
                       removal_reason = removal_reason,
                       infections = infections,
                       t0 = t0,
                       t1 = t1,
                       total_exposure_days = total_exposure_days,
                       n_peritonitis_episodes = n_peritonitis_episodes,
                       peritonitis_flag = peritonitis_flag
                       )

  validate_pd_catheter(x)
}

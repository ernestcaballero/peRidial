

#' Create pd_catheter object
#'
#' A single PD catheter episode for a patient: when it was inserted,
#' the window of active PD therapy, and (if applicable) why and
#' when it was removed.
#'
#' @param patient_id Character. The patient's unique identifier (NHI)/
#' @param catheter_id Character. Unique identifier for this catheter.
#' @param insertion_date Date. Date the cathetee was surgically inserted.
#' @param procedure_type Character. Tyoe of insertion technique (eg. \code{"open surgical"},
#'    \code{"laparoscopic"}, \code{"percutaneous"}).
#' @param pd_start_date Date. Date of start of PD therapy of this catheter,
#'    usually 2-4 weeks after insertion.
#' @param pd_stop_date Date. Date PD therapy stopped on this catheter, or
#'   \code{NA} if the catheter is still in active use.
#' @param removal_reason Character. Reason the catheter was removed (e.g. \code{"infection"},
#'   \code{"mechanical failure"}, \code{"transplant"}), or \code{NA} if it hasn't been removed.)
#'
#' @returns An object of class \code{pd_catheter}.
#' @export
#'
#'
new_pd_catheter <- function(patient_id = NA_character_,
                            catheter_id = NA_character_,
                            insertion_date = as.Date(NA),
                            procedure_type = NA_character_,
                            pd_start_date = as.Date(NA),
                            pd_stop_date = as.Date(NA),
                            removal_reason = NA_character_) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(patient_id))
  stopifnot(length(catheter_id) == 1, is.character(catheter_id) || is.na(catheter_id))
  stopifnot(inherits(insertion_date, "Date"))
  stopifnot(is.character(procedure_type) || is.na(procedure_type))
  stopifnot(inherits(pd_start_date, "Date"))
  stopifnot(inherits(pd_stop_date, "Date"))
  stopifnot(is.character(removal_reason) || is.na(removal_reason))

  structure(
    list(
      patient_id = patient_id,
      catheter_id = catheter_id,
      insertion_date = insertion_date,
      procedure_type = procedure_type,
      pd_start_date = pd_start_date,
      pd_stop_date = pd_stop_date,
      removal_reason = removal_reason
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

  x
}



#' Construct and validate a pd_catheter object
#'
#' User-facing constructor: builds a \code{pd_catheter} via
#' \code{new_pd_catheter()} and checks it with \code{validate_pd_catheter()}
#' before returning it.
#'
#' @inheritParams new_pd_catheter
#' @param patient_id Character. The patient's unique identifier (NHI).
#' @param catheter_id Character. Unique identifier for this catheter.
#' @param insertion_date Date. Date the catheter was surgically inserted.
#' @param pd_start_date Date. Date PD therapy started on this catheter.
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
                        removal_reason = NA_character_) {
  x <- new_pd_catheter(patient_id = patient_id,
                       catheter_id = catheter_id,
                       insertion_date = insertion_date,
                       procedure_type = procedure_type,
                       pd_start_date = pd_start_date,
                       pd_stop_date = pd_end_date,
                       removal_reason = removal_reason
                       )

  validate_pd_catheter(x)
}

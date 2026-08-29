

#' Create pd_infection object
#'
#' Peritonitis-level infection data for any catheter owned by a patient in the
#' reporting period. Includes date of infection, peritonitis episode type,
#' causative organism, and outcome (if resolved, catheter removed or transferred
#' to haemodialysis - permanently or temporarily).
#'
#' @param patient_id Character. The patient's unique identifier (NHI).
#' @param infection_date Date. The date the peritonitis episode was diagnosed.
#' @param organism_list  List. A list of one or more character strings of the
#'   causative organism(s) identified for this episode. Use \code{"negative"}
#'   for a culture-negative episode; \code{"negative"} cannot be combined with
#'   other organisms in the same list.
#' @param episode_type Character. One of \code{"relapsing"}, \code{"recurrent"},
#'   or \code{"repeat"} (per ISPD 2022 definitions), or \code{NA} if this is the
#'   patient's first recorded episode and there is no prior episode to compare
#'   against. Typically derived via \code{get_episode_type()}.
#' @param last_dose_antibiotic Date. Date of the last dose of antibiotic
#'   treatment given for this episode.
#' @param outcome Character. Clinical outcome of this episode (e.g.
#'   \code{"hospitalisation"}, \code{"catheter removed"}, \code{"temporary transfer to HD"}).
#'   If supplied, \code{outcome_date} must also be supplied.
#' @param outcome_date Date. Date of the outcome. Required whenever
#'   \code{outcome} is supplied, and specifically required when \code{outcome} is \code{"catheter removed"}.
#'
#' @returns An object of class \code{pd_infection}.
#' @export
#'
#' @examples
#'
#'
#'
#'

new_pd_infection <- function(patient_id = NA_character_,
                             # catheter_id = NA_character_,
                             infection_date = as.Date(NA),
                             organism_list = list(),
                             episode_type = NA_character_,
                             last_dose_antibiotic = as.Date(NA),
                             outcome = NA_character_,
                             outcome_date = as.Date(NA)) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(patient_id))
  # stopifnot(length(catheter_id) == 1, is.character(catheter_id) || is.na(catheter_id))
  stopifnot(inherits(infection_date, "Date"))
  stopifnot(is.list(organism_list))
  stopifnot(is.character(episode_type) || is.na(episode_type))
  stopifnot(inherits(last_dose_antibiotic, "Date"))
  stopifnot(is.character(outcome) || is.na(outcome))
  stopifnot(inherits(outcome_date, "Date"))

  structure(
    list(
      patient_id = patient_id,
      # catheter_id = catheter_id,
      infection_date = infection_date,
      organism_list = organism_list,
      episode_type = episode_type,
      last_dose_antibiotic = last_dose_antibiotic,
      outcome = outcome,
      outcome_date = outcome_date
    ),
    class = "pd_infection"
  )
}



validate_pd_infection <- function(x) {
  stopifnot(inherits(x, "pd_infection"))

  # if (is.na(x$patient_id) || is.na(x$catheter_id)) {
  #   stop("Both patient_id and catheter_id must be supplied.")
  # }
  if (is.na(x$patient_id)) {
    stop("patient_id and catheter_id must be supplied.")
  }
  if (is.na(x$infection_date)) {
    stop("Missing infection date. Must be supplied.")
  }
  # Validates for negative-culture peritonitis; this field must be completed
  if (length(x$organism_list) == 0 || anyNA(x$organism_list)) {
    stop("At least one organism must be supplied in organism_list; if none was idenitifed, use 'negative'")
  }
  # Ensures 'negative' can't be with other organisms in the list
  if (any(tolower(unlist(x$organism_list)) == "negative") && length(x$organism_list) > 1) {
    stop("'negative' cannot be combined with other organisms in the organism_list.")
  }
  # episode_type is optional; if supplied, must be a valid category
  if (!is.na(x$episode_type) && !(x$episode_type %in% c("relapsing", "recurrent", "repeat"))) {
    stop("episode_type, if supplied, must be one of 'relapsing', 'recurrent', 'repeat'.")
  }
  if (is.na(x$last_dose_antibiotic)) {
    stop("Missing date of last dose of antibiotic. Must be supplied.")
  }
  # No outcome (implied good recovery), but outcome_date supplied — invalid
  if (is.na(x$outcome) && !is.na(x$outcome_date)) {
    stop("outcome_date must be matched with an outcome.")
  }
  # "Catheter removed" outcome requires a date
  if (!is.na(x$outcome) && tolower(x$outcome) == "catheter removed" && is.na(x$outcome_date)) {
    stop("Missing date of catheter removal. Must be supplied.")
  }
  x
}



#' Derive episode type for a peritonitis episode
#'
#' Classifies a peritonitis episode as \code{"relapsing"}, \code{"recurrent"}, \code{"repeat"}
#' based on its organism(s) and timing relative to the immediately preceding episode's antibiotic
#' completion date, per ISPD 2022 definitions.
#'
#' @param current_infection_date Date. Infection date of the episode being classified.
#' @param current_organism_list A list of organism name(s) for this episode.
#' @param prior_episode A \code{pd_infection} object for the same patient's
#'   immediately preceding episode, or \code{NULL} if this is their first
#'   recorded episode.
#'
#' @returns Character: one of \code{"relapsing"}, \code{"recurrent"},
#'   \code{"repeat"}, or \code{NA_character_}.
#'
get_episode_type <- function(current_infection_date,
                             current_organism_list,
                             prior_episode = NULL) {
  stopifnot(inherits(current_infection_date, "Date"))
  stopifnot(is.list(current_organism_list))

  # checks if first recorded episode — nothing to compare against
  if (is.null(prior_episode)) {
    return(NA_character_)
  }

  stopifnot(inherits(prior_episode, "pd_infection"))

  # can't judge the 4-week window without this date
  if (is.na(prior_episode$last_dose_antibiotic)) {
    return(NA_character_)
  }

  days_since_treatment <- as.numeric(current_infection_date - prior_episode$last_dose_antibiotic)
  within_4_weeks <- days_since_treatment <= 28               # LOGICAL: TRUE if less than or equal to 28 days/4 weeks

  same_organism <- identical(
    sort(tolower(unlist(current_organism_list))),
    sort(tolower(unlist(prior_episode$organism_list)))
  )

  if (within_4_weeks && same_organism) {
    "relapsing"
  } else if (within_4_weeks && !same_organism) {
    "recurrent"
  } else if (!within_4_weeks && same_organism) {
    "repeat"
  } else {
    NA_character_   # different organism, >4 weeks later — not an ISPD-defined category
  }
}





#' Construct and validate a pd_infection object
#'
#' Builds a \code{pd_infection} via \code{new_pd_infection()} and checks it with
#' \code{validate_pd_infection()} before returning it.
#'
#' @inheritParams new_pd_infection
#'
#' @return An object of class \code{pd_infection}.
#' @export
#'

pd_infection <- function(patient_id,
                         # catheter_id,
                         infection_date,
                         organism_list,
                         last_dose_antibiotic = as.Date(NA),
                         outcome = NA_character_,
                         outcome_date = as.Date(NA),
                         prior_episode = NULL) {
  episode_type <- get_episode_type(current_infection_date = infection_date,
                                   current_organism_list = organism_list,
                                   prior_episode = prior_episode
                                   )

  x <- new_pd_infection(
    patient_id = patient_id,
    # catheter_id = catheter_id,
    infection_date = infection_date,
    organism_list = organism_list,
    episode_type = episode_type,
    last_dose_antibiotic = last_dose_antibiotic,
    outcome = outcome,
    outcome_date = outcome_date
  )

  validate_pd_infection(x)
}



#' Create pd_infection object
#'
#' Peritonitis episode data for any catheter owned by a patient in the reporting period.
#'
#' @param patient_id Character. The patient's unique identifier (NHI).
#' @param infection_date Date. The date the peritonitis episode was diagnosed.
#' @param organism_list  List. A list of one or more character strings of the
#'   causative organism(s) identified for this episode. Use \code{"negative"}
#'   for a culture-negative episode; \code{"negative"} cannot be combined with
#'   other organisms in the same list.
#' @param episode_type Character. One of \code{"relapsing"}, \code{"recurrent"}
#'   or \code{"repeat"} (per ISPD 2022 definitions), or \code{NA} when no ISPD
#'   category applies. \code{NA} is not a missing value and the episode is still
#'   counted towards the peritonitis rate; see \code{\link{get_episode_type}()}.
#'   Derived via \code{\link{get_episode_type}()}.
#' @param last_dose_antibiotic Date. Date of the last dose of antibiotic
#'   treatment given for this episode. Required: it is the date every later
#'   episode of the same patient is classified against.
#' @param outcome Character. Clinical outcome of this episode, one of
#'   \code{"catheter removed"}, \code{"permanent transfer to HD"},
#'   \code{"temporary transfer to HD"} or \code{"hospitalisation"} (when
#'   \code{pd_unit()} derives it, the most severe flag set on the PE row wins,
#'   in that order). \code{NA} means the episode resolved, and is not a missing
#'   value.
#' @param outcome_date Date. Date of the outcome. Required when \code{outcome}
#'   is \code{"catheter removed"}; optional for the other outcomes, and in fact
#'   left \code{NA} by \code{pd_unit()} for \code{"hospitalisation"}. An
#'   \code{outcome_date} without an \code{outcome} is an error.
#'
#' @return An object of class \code{pd_infection}.
#' @export
#'
#' @examples
#' \dontrun{
#' # the unchecked constructor; pd_infection() is the one that validates
#' new_pd_infection(
#'   patient_id = "ABC0110",
#'   infection_date = as.Date("2025-06-01"),
#'   organism_list = list("S. aureus"),
#'   episode_type = NA_character_,
#'   last_dose_antibiotic = as.Date("2025-06-15"),
#'   outcome = NA_character_,
#'   outcome_date = as.Date(NA)
#' )}
#'

new_pd_infection <- function(patient_id = NA_character_,
                             infection_date = as.Date(NA),
                             organism_list = list(),
                             episode_type = NA_character_,
                             last_dose_antibiotic = as.Date(NA),
                             outcome = NA_character_,
                             outcome_date = as.Date(NA)) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(patient_id))
  stopifnot(inherits(infection_date, "Date"))
  stopifnot(is.list(organism_list))
  stopifnot(is.character(episode_type) || is.na(episode_type))
  stopifnot(inherits(last_dose_antibiotic, "Date"))
  stopifnot(is.character(outcome) || is.na(outcome))
  stopifnot(inherits(outcome_date, "Date"))

  structure(
    list(
      patient_id = patient_id,
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

  if (is.na(x$patient_id)) {
    stop("patient_id must be supplied.")
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
#' \strong{Relapsing}: within 4 weeks of completing therapy for the prior
#' episode, AND either the same specific organism as the prior episode, or a
#' culture-negative/specific-organism pairing in either direction (negative
#' then specific, specific then negative, or negative then negative).
#'
#' \strong{Recurrent}: within 4 weeks of completing therapy, but with a
#' different specific organism to the prior episode (and not a
#' culture-negative pairing).
#'
#' \strong{Repeat}: more than 4 weeks after completing therapy, with the same
#' specific organism as the prior episode.
#'
#' \strong{NA}: no ISPD category applies. This is not a missing value, and the
#' episode is still counted towards the peritonitis rate. It arises three ways:
#' the patient's first recorded episode (\code{prior_episode = NULL}); a
#' different organism more than 4 weeks after completing therapy; or a prior
#' episode whose \code{last_dose_antibiotic} is missing, so the 4-week window
#' cannot be judged.
#'
#' The 4-week boundary is inclusive: 28 days after the prior episode's last
#' dose is within 4 weeks. Organisms are compared ignoring case
#' and order, and two episodes are the same organism only if their whole
#' organism lists match.
#'
#' @param current_infection_date Date. Infection date of the episode being classified.
#' @param current_organism_list A list of organism name(s) for this episode.
#' @param prior_episode A \code{pd_infection} object for the same patient's
#'   immediately preceding episode, or \code{NULL} if this is their first
#'   recorded episode.
#'
#' @return Character: one of \code{"relapsing"}, \code{"recurrent"},
#'   \code{"repeat"}, or \code{NA_character_}.
#' @seealso \code{\link{classify_episode_types}()} for many episodes at once,
#'   and \code{vignette("indicators-classification", package = "peridial")}.
#' @export
#'
#' @examples
#' \dontrun{
#' first <- pd_infection("ABC1234", as.Date("2025-04-01"), list("E. coli"),
#'                       last_dose_antibiotic = as.Date("2025-04-15"))
#'
#' # 27 days after the last dose, same organism
#' get_episode_type(as.Date("2025-05-12"), list("E. coli"), prior_episode = first)
#'
#' # same timing, a different organism
#' get_episode_type(as.Date("2025-05-12"), list("Klebsiella"), prior_episode = first)
#'
#' # same organism, but more than 4 weeks later
#' get_episode_type(as.Date("2025-07-01"), list("E. coli"), prior_episode = first)
#'
#' # a patient's first episode has nothing to be classified against
#' get_episode_type(as.Date("2025-04-01"), list("E. coli"), prior_episode = NULL)
#' }
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

  current_organisms <- sort(tolower(unlist(current_organism_list)))
  prior_organisms <- sort(tolower(unlist(prior_episode$organism_list)))

  same_organism <- identical(current_organisms, prior_organisms)
  current_negative <- identical(current_organisms, "negative")
  prior_negative <- identical(prior_organisms, "negative")

  is_relapse_pair <- same_organism || current_negative || prior_negative

  if (within_4_weeks && is_relapse_pair) {
    "relapsing"
  } else if (within_4_weeks && !is_relapse_pair) {
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
#' \code{validate_pd_infection()} before returning it. \code{episode_type} is
#' derived automatically from \code{prior_episode} via \code{get_episode_type()}.
#'
#' @inheritParams new_pd_infection
#' @param prior_episode A \code{pd_infection} object for the same patient's
#'   immediately preceding episode, or \code{NULL} if this is their first
#'   recorded episode. Passed straight through to \code{get_episode_type()}
#'   to derive this episode's \code{episode_type}.
#'
#' @return An object of class \code{pd_infection}.
#' @seealso \code{\link{get_episode_type}()} for the classification rule, and
#'   \code{vignette("data-structures", package = "peridial")}.
#' @export
#'
#' @examples
#' \dontrun{
#' first <- pd_infection(
#'   patient_id = "ABC1234",
#'   infection_date = as.Date("2025-04-01"),
#'   organism_list = list("E. coli"),
#'   last_dose_antibiotic = as.Date("2025-04-15")
#' )
#' first$episode_type   # NA: there is nothing prior to classify against
#'
#' # the same patient's next episode, classified against the first
#' pd_infection(
#'   patient_id = "ABC1234",
#'   infection_date = as.Date("2025-05-12"),
#'   organism_list = list("E. coli"),
#'   last_dose_antibiotic = as.Date("2025-05-26"),
#'   prior_episode = first
#' )
#'
#' # every episode needs a last antibiotic dose
#' try(pd_infection("ABC1234", as.Date("2025-03-10"), list("S. aureus")))
#' }
#'
pd_infection <- function(patient_id,
                         infection_date,
                         organism_list,
                         last_dose_antibiotic = as.Date(NA),
                         outcome = NA_character_,
                         outcome_date = as.Date(NA),
                         prior_episode = NULL) {
  episode_type <- get_episode_type(current_infection_date = infection_date,
                                   current_organism_list = organism_list,
                                   prior_episode = prior_episode)

  x <- new_pd_infection(
    patient_id = patient_id,
    infection_date = infection_date,
    organism_list = organism_list,
    episode_type = episode_type,
    last_dose_antibiotic = last_dose_antibiotic,
    outcome = outcome,
    outcome_date = outcome_date
  )

  validate_pd_infection(x)
}




#' Print a pd_infection object
#'
#' A single peritonitis episode's snapshot: patient id, infection date,
#' causative organism(s), episode type, date of last antibiotic dose, and
#' outcome.
#'
#' @param x A \code{pd_infection} object.
#' @param ... Ignored.
#'
#' @return \code{x}, invisibly.
#' @export
#'
#' @examples
#' # an NA outcome means the episode resolved, not that the outcome is missing
#' pd_infection("ABC1234", as.Date("2025-03-01"), list("S. aureus"),
#'              last_dose_antibiotic = as.Date("2025-03-15"))
#'
print.pd_infection <- function(x, ...) {
  cat("<pd_infection>", if (is.na(x$patient_id)) "(unknown patient)" else x$patient_id, "\n")
  cat(sprintf("  %-21s: %s\n", "Infection date", format(x$infection_date)))
  cat(sprintf("  %-21s: %s\n", "Organism/s", paste(unlist(x$organism_list), collapse = ", ")))
  cat(sprintf("  %-21s: %s\n", "Episode type", if (is.na(x$episode_type)) NA_character_ else x$episode_type))
  cat(sprintf("  %-21s: %s\n", "Last dose antibiotic", format(x$last_dose_antibiotic)))
  cat(sprintf("  %-21s: %s\n", "Outcome", if (is.na(x$outcome)) "NA" else x$outcome))
  invisible(x)
}

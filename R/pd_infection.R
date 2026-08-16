

#' Create pd_infection object
#'
#' Peritonitis-level infection data for any catheter owned by a patient in the
#' reporting period. Includes date of infection, peritonitis episode type,
#' causative organism, and outcome (if resolved, catheter removed or transferred
#' to haemodialysis - permanently or temporarily).
#'
#' @param patient_id
#' @param catheter_id
#' @param infection_date
#' @param organism_list
#' @param episode_type
#' @param last_dose_antibiotic
#' @param outcome
#' @param outcome_date
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
                             catheter_id = NA_character_,
                             infection_date = as.Date(NA),
                             organism_list = list(),
                             episode_type = NA_character_,
                             last_dose_antibiotic = as.Date(NA),
                             outcome = NA_character_,
                             outcome_date = as.Date(NA)) {

  stopifnot(length(patient_id) == 1, is.character(patient_id) || is.na(unit_id))
  stopifnot(length(catheter_id) ==1, is.character(catheter_id) || is.na(catheter_id))
  stopifnot(inherits(infection_date, "Date"), inherits(last_dose_antibiotic, "Date"),
            inherits(outcome_date, "Date"))

  structure(
    list(
      patient_id = patient_id,
      catheter_id = catheter_id,
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

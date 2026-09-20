#' Classify peritonitis episode types for many episodes at once
#'
#' A vectorised counterpart to [get_episode_type()]. Each row is first checked
#' exactly as [pd_infection()] would check it, by building a `pd_infection`
#' with [new_pd_infection()] and passing it through `validate_pd_infection()`.
#' Valid rows are then classified as \code{"relapsing"}, \code{"recurrent"} or \code{"repeat"}
#' (ISPD 2022 definitions) against the same patient's immediately preceding
#' valid episode. The classification loop runs in C++.
#'
#' @param patient_id Character. Patient identifier for each episode.
#' @param infection_date Date. Date each episode was diagnosed.
#' @param last_dose_antibiotic Date. Date of the last antibiotic dose for each
#'   episode.
#' @param organism_list List, one element per episode, each itself a list of
#'   that episode's organism name(s). Use \code{"negative"} for culture-negative.
#' @param outcome Character. Outcome of each episode, or \code{NA} when resolved.
#'   Defaults to all \code{NA}.
#' @param outcome_date Date. Date of each outcome. Defaults to all \code{NA}.
#'
#' @returns Character vector, the same length as the inputs: \code{"relapsing"},
#'   \code{"recurrent"}, \code{"repeat"}, or \code{NA_character_} (first valid
#'   episode for a patient, invalid row, or no ISPD category applies).
#' @export
classify_episode_types <- function(patient_id,
                                   infection_date,
                                   last_dose_antibiotic,
                                   organism_list,
                                   outcome = rep(NA_character_, length(patient_id)),
                                   outcome_date = rep(as.Date(NA), length(patient_id))) {
  n <- length(patient_id)
  stopifnot(inherits(infection_date, "Date"),
            inherits(last_dose_antibiotic, "Date"),
            inherits(outcome_date, "Date"),
            is.list(organism_list),
            length(infection_date) == n,
            length(last_dose_antibiotic) == n,
            length(organism_list) == n,
            length(outcome) == n,
            length(outcome_date) == n
            )

  # validate first
  valid <- vapply(seq_len(n), function(i) {
    tryCatch({
      validate_pd_infection(new_pd_infection(
        patient_id = patient_id[i],
        infection_date = infection_date[i],
        organism_list = organism_list[[i]],
        episode_type = NA_character_,
        last_dose_antibiotic = last_dose_antibiotic[i],
        outcome = outcome[i],
        outcome_date = outcome_date[i]
      ))
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))

  # one comparable string per episode, same as get_episode_type() - eg. lowercase and sorted
  key <- vapply(organism_list,
                function(x) paste(sort(tolower(unlist(x))), collapse = "|"),
                character(1)
                )

  # Rcpp implementation of classify_episode_types_cpp
  ord <- order(patient_id, infection_date)   # grouped by patient and oldest infection first
  out <- character(n)
  out[ord] <- classify_episode_types_cpp(
    patient_id = as.character(patient_id)[ord],
    infection_date = as.numeric(infection_date)[ord],
    last_dose_antibiotic = as.numeric(last_dose_antibiotic)[ord],
    organism_key = key[ord],
    valid = valid[ord]
  )
  out
}

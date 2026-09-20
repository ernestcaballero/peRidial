

classify_episode_types <- function(patient_id,
                                   infection_date,
                                   last_dose_antibiotic,
                                   organism_list,
                                   outcome = rep(NA_character_, length(patient_id)),
                                   outcome_date = rep(as.Date(NA), length(patient_id)) {
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
        organism_list = organism_list[i],
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

  # C++ (grouped by patient and oldest infection first)
  ord <- order(patient_id, infection_date)
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

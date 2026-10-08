
# These are helper functions to flatten patients, catheters, and infections to the units tibbles


#' Flatten patient_list into the unit's patients tibble
#' @noRd
patients_to_tibble <- function(patient_list, t0, t1, details = character(0)) {
  if (length(patient_list) == 0) {
    return(tibble::tibble(
      patient_id = character(0), t0 = as.Date(character(0)),
      t1 = as.Date(character(0)), new_patient_flag = logical(0),
      n_catheters = integer(0), n_episodes = numeric(0),
      gender = character(0), ethnicity = character(0),
      date_of_birth = as.Date(character(0)),
      primary_kidney_disease = character(0), diabetes_status = character(0),
      smoking_status = character(0), dialysis_type = character(0),
      transfer_reason = character(0), transfer_date = as.Date(character(0)),
      transfer_detail = character(0),
      first_pd_start_date = as.Date(character(0)),
      patient_years_at_risk = numeric(0)
    ))
  }

  # computed before tibble(), which evaluates its arguments in a data mask
  ids <- vapply(patient_list, function(p) p$patient_id, character(1))

  first_start <- do.call(c, lapply(patient_list, function(p) {
    starts <- do.call(c, lapply(p$catheters, function(cath) cath$pd_start_date))
    if (is.null(starts) || length(starts) == 0) as.Date(NA) else min(starts)
  }))

  # per-patient contribution to the unit's tpyar denominator
  py <- vapply(patient_list, function(p) total_patient_years(list(p), t0, t1),
               numeric(1))

  tibble::tibble(
    patient_id = ids,
    t0 = rep(t0, length(patient_list)),
    t1 = rep(t1, length(patient_list)),
    new_patient_flag = vapply(patient_list,
                              function(p) as.logical(p$new_patient_flag),
                              logical(1)),
    n_catheters = vapply(patient_list,
                         function(p) as.integer(p$n_catheters), integer(1)),
    n_episodes = vapply(patient_list,
                        function(p) as.numeric(p$n_episodes), numeric(1)),
    gender = vapply(patient_list, function(p) as.character(p$gender), character(1)),
    ethnicity = vapply(patient_list, function(p) as.character(p$ethnicity), character(1)),
    date_of_birth = do.call(c, lapply(patient_list, function(p) p$date_of_birth)),
    primary_kidney_disease = vapply(patient_list,
                                    function(p) as.character(p$primary_kidney_disease),
                                    character(1)),
    diabetes_status = vapply(patient_list,
                             function(p) as.character(p$diabetes_status), character(1)),
    smoking_status = vapply(patient_list,
                            function(p) as.character(p$smoking_status), character(1)),
    dialysis_type = vapply(patient_list,
                           function(p) as.character(p$dialysis_type), character(1)),
    transfer_reason = vapply(patient_list,
                             function(p) as.character(p$transfer_reason),
                             character(1)),
    transfer_date = do.call(c, lapply(patient_list, function(p) p$transfer_date)),
    transfer_detail = unname(ifelse(ids %in% names(details),
                                    details[ids], NA_character_)),
    first_pd_start_date = first_start,
    patient_years_at_risk = py
  )
}




#' Flatten every catheter in patient_list into the unit's catheters tibble
#' @noRd
catheters_to_tibble <- function(patient_list, t0, t1) {
  caths <- unlist(lapply(patient_list, function(p) p$catheters),
                  recursive = FALSE, use.names = FALSE)

  if (length(caths) == 0) {
    return(tibble::tibble(
      patient_id = character(0), catheter_id = character(0),
      insertion_date = as.Date(character(0)), procedure_type = character(0),
      pd_start_date = as.Date(character(0)), pd_stop_date = as.Date(character(0)),
      removal_reason = character(0), n_peritonitis_episodes = numeric(0),
      peritonitis_flag = logical(0), exposure_days_in_period = numeric(0)
    ))
  }

  # each catheter's tau, so exposure days are censored the same way tpyar is
  taus <- unlist(lapply(patient_list, function(p) {
    rep(list(p$transfer_date), length(p$catheters))
  }), recursive = FALSE, use.names = FALSE)

  # computed before tibble(), which evaluates its arguments in a data mask
  exposure <- vapply(seq_along(caths), function(i) {
    catheter_exposure_days(caths[[i]], t0, t1, tau = taus[[i]])
  }, numeric(1))

  tibble::tibble(
    patient_id = vapply(caths, function(z) z$patient_id, character(1)),
    catheter_id = vapply(caths, function(z) z$catheter_id, character(1)),
    insertion_date = do.call(c, lapply(caths, function(z) z$insertion_date)),
    procedure_type = vapply(caths, function(z) as.character(z$procedure_type),
                            character(1)),
    pd_start_date = do.call(c, lapply(caths, function(z) z$pd_start_date)),
    pd_stop_date = do.call(c, lapply(caths, function(z) z$pd_stop_date)),
    removal_reason = vapply(caths, function(z) as.character(z$removal_reason),
                            character(1)),
    n_peritonitis_episodes = vapply(caths,
                                    function(z) as.numeric(z$n_peritonitis_episodes),
                                    numeric(1)),
    peritonitis_flag = vapply(caths, function(z) as.logical(z$peritonitis_flag),
                              logical(1)),
    exposure_days_in_period = exposure
  )
}


#' Flatten every peritonitis episode into the unit's infections tibble
#' @noRd
infections_to_tibble <- function(patient_list, t0, t1) {
  rows <- list()

  for (p in patient_list) {
    for (cath in p$catheters) {
      for (inf in cath$infections) {
        # an episode counts towards the rate if it is inside this catheter's active window within [t0, t1] and is not a relapse
        counts <- count_episodes_in_period(list(inf), t0, t1,
                                           cath$pd_start_date,
                                           cath$pd_stop_date) > 0
        rows[[length(rows) + 1]] <- tibble::tibble(
          patient_id = inf$patient_id,
          catheter_id = cath$catheter_id,
          infection_date = inf$infection_date,
          episode_type = as.character(inf$episode_type),
          organisms = paste(unlist(inf$organism_list), collapse = ", "),
          n_organisms = length(inf$organism_list),
          last_dose_antibiotic = inf$last_dose_antibiotic,
          outcome = as.character(inf$outcome),
          outcome_date = inf$outcome_date,
          counts_toward_rate = counts
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      patient_id = character(0), catheter_id = character(0),
      infection_date = as.Date(character(0)), episode_type = character(0),
      organisms = character(0), n_organisms = integer(0),
      last_dose_antibiotic = as.Date(character(0)), outcome = character(0),
      outcome_date = as.Date(character(0)), counts_toward_rate = logical(0)
    ))
  }

  out <- do.call(rbind, rows)
  out[order(out$patient_id, out$infection_date), ]
}





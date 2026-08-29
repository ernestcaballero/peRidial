#' Build pd_unit object from raw data files
#'
#' Reads a unit's raw dialysis data file and infection data file
#' and builds patient, catheter, and peritonitis-episode data
#' for a reporting period.
#'
#' @param unit_data_path Character. Path to the raw unit Excel file
#'   containing both patient and catheter data.
#' @param infection_data_path Character. Path to the raw infection/peritonitis
#'   episode Excel file.
#' @param t0 Date. Start of the reporting period.
#' @param t1 Date. End of the reporting period.
#' @param unit_id Character. Optional identifier for the unit. Defaults to
#'   \code{NA_character_} if not supplied.
#'
#' @return A \code{pd_unit} object containing the patient, catheter, and
#'   peritonitis episode data for the unit over the reporting period
#'   \code{t0} to \code{t1}.
#' @export
#'

pd_unit <- function(unit_data_path,
                    infection_data_path,
                    t0,
                    t1,
                    unit_id = NA_character_) {

  stopifnot(inherits(t0, "Date"), inherits(t1, "Date"))

  # Read raw files, change date format %d-%m-%Y
  raw_a3 <- readxl::read_excel(unit_data_path)

  # standardise column names here (can use function)

  raw_patients <- raw_a3 |>
    dplyr::select(patient_id, date_of_birth, gender, ethnicity,
                  primary_kidney_disease_code, height, weight,
                  cigarette_smoking_status, diabetes_type,
                  modality_change_reason_code,
                  date_of_death, cause_of_death,
                  current_centre_name, date_transfer_current,
                  last_centre_name, date_transfer_last,
                  dialysis_type_code, dry_weight_at_last_dx_kg) |>
    dplyr::mutate(
      date_of_birth = as.Date(date_of_birth, format = "%d-%m-%Y"),
      date_of_death = as.Date(date_of_death, format = "%d-%m-%Y"),
      date_transfer_current = as.Date(date_transfer_current, format = "%d-%m-%Y"),
      date_transfer_last = as.Date(date_transfer_last, format = "%d-%m-%Y")) |>
    dplyr::distinct(patient_id, .keep_all = TRUE)

  raw_catheters <- raw_a3 |>
    dplyr::select(patient_id, insertion_date, procedure_type,
                  pd_start_date, pd_stop_date, removal_reason,
                  peritonitis_date_first_episode,
                  peritonitis_episodes_count) |>
    dplyr::mutate(
      insertion_date = as.Date(insertion_date, format = "%d-%m-%Y"),
      pd_start_date = as.Date(pd_start_date, format = "%d-%m-%Y"),
      pd_stop_date = as.Date(pd_stop_date, format = "%d-%m-%Y"),
      peritonitis_date_first_episode = as.Date(peritonitis_date_first_episode, format = "%d-%m-%Y")
    )

  raw_pe <- readxl::read_excel(infection_data_path) |>
  dplyr::select(patient_id, catheter_id, date_of_infection,
                relapse_recurrence_code, organism, last_dose_antibiotic,
                overnight_hospitalisation, days_hospitalised,
                catheter_removed, catheter_removed_date,
                interim_hd, permanent_hd, first_dialysis_date, last_dialysis_date) |>
  dplyr::mutate(
    date_of_infection = as.Date(date_of_infection, format = "%d-%m-%Y"),
    last_dose_antibiotic = as.Date(last_dose_antibiotic, format = "%d-%m-%Y"),
    catheter_removed_date = as.Date(catheter_removed_date, format = "%d-%m-%Y"),
    first_dialysis_date = as.Date(first_dialysis_date, format = "%d-%m-%Y"),
    last_dialysis_date = as.Date(last_dialysis_date, format = "%d-%m-%Y")
  )

  # create catheter_id for raw_pe

  # build pd_catheter object and add catheter_id from pd_infection for infection within [t0, t1]

  # Derive organism_list and outcome for each episode
  # ASSUMPTION: raw_pe already has one row per peritonitis episode.
  # Multiple organism peritonitis are made into a list in a single column
  # as a comma-separated string (e.g. "E. coli, Staph aureus") -- that
  # gets split into a list column here.
  #
  # ASSUMPTION: outcome priority is catheter removal > permanent HD >
  # temporary HD > hospitalisation, i.e. if more than one outcome flag is
  # set for an episode the "most severe" one wins.
  #
  # outcome/outcome_date are deliberately nullable: an episode that resolves
  # without catheter removal, HD transfer, or hospitalisation has none of
  # those flags set, so it falls through both case_when()s to NA. That's
  # not a missing-data gap -- it's the "peritonitis resolved" case, and
  # validate_pd_infection() already treats NA outcome as implied good
  # recovery (it only errors if outcome_date is set without an outcome, or
  # if outcome == "catheter removed" is set without a date).

  # Build a label per row by checking if catheter removed, permanent HD, interim HD, hospitalisation or none/resolved
  raw_pe_episodes <- raw_pe |>
    dplyr::mutate(
      organism_list = lapply(strsplit(organism, ",\\s*"), trimws),
      outcome = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE)           ~ "catheter removed",
        dplyr::coalesce(permanent_hd, FALSE)               ~ "permanent transfer to HD",
        dplyr::coalesce(interim_hd, FALSE)                 ~ "temporary transfer to HD",
        dplyr::coalesce(overnight_hospitalisation, FALSE)  ~ "hospitalisation",
        TRUE ~ NA_character_
      ),
      # Populates outcome date if outcome is specified
      outcome_date = dplyr::case_when(
        dplyr::coalesce(catheter_removed, FALSE) ~ catheter_removed_date,
        dplyr::coalesce(permanent_hd, FALSE) | dplyr::coalesce(interim_hd, FALSE) ~ first_dialysis_date,
        TRUE ~ as.Date(NA)
      )
    ) |>
    # Sorts the table by patient first, then infection date/s within each patient
    dplyr::arrange(patient_id, date_of_infection)


  # Build infection objects by patients
  # Using chaining of episodes chronologically per patient
  # get_episode_type() (see pd_infection.R) needs the immediately preceding
  # episode for the same patient, so this has to walk each patient's
  # episodes in date order, feeding each pd_infection() call the object
  # built just before it.

  build_patient_infections <- function(df) {
    infections <- vector("list", nrow(df))
    prior <- NULL
    for (i in seq_len(nrow(df))) {
      infections[[i]] <- pd_infection(
        patient_id = df$patient_id[i],
        catheter_id = df$catheter_id[i],
        infection_date = df$date_of_infection[i],
        organism_list = df$organism_list[[i]],
        last_dose_antibiotic = df$last_dose_antibiotic[i],
        outcome = df$outcome[i],
        outcome_date = df$outcome_date[i],
        prior_episode = prior
      )
      prior <- infections[[i]]
    }
    infections
  }

  infections_by_patient <- raw_pe_episodes |>
    split(raw_pe_episodes$patient_id) |>
    purrr::map(build_patient_infections)

  # Regroup the same pd_infection objects by catheter_id instead of
  # patient_id, since pd_catheter (not pd_patient) owns its infections --
  # see the composition in the design proposal's Figure 2. The
  # patient-chronological build above is still needed first: get_episode_type()
  # has to compare each episode against the *patient's* immediately
  # preceding episode, which can span a catheter change.
  all_infections <- unlist(infections_by_patient, recursive = FALSE, use.names = FALSE)

  infections_by_catheter <- if (length(all_infections) > 0) {
    split(all_infections, vapply(all_infections, function(inf) inf$catheter_id, character(1)))
  } else {
    list()
  }


  # Build catheter objects per patient, attaching each catheter's own
  # infections from infections_by_catheter
  build_patient_catheters <- function(pid) {
    df <- raw_catheters[raw_catheters$patient_id == pid, ]
    purrr::pmap(df, function(patient_id, catheter_id, insertion_date,
                             procedure_type, pd_start_date, pd_stop_date,
                             removal_reason, peritonitis_date_first_episode,
                             peritonitis_episodes_count) {
      cath_infections <- infections_by_catheter[[catheter_id]]
      if (is.null(cath_infections)) cath_infections <- list()

      pd_catheter(
        patient_id = patient_id,
        catheter_id = catheter_id,
        insertion_date = insertion_date,
        procedure_type = procedure_type,
        pd_start_date = pd_start_date,
        pd_stop_date = pd_stop_date,
        removal_reason = removal_reason,
        infections = cath_infections,
        # t0/t1 come from pd_unit()'s own arguments (in scope here as a
        # closure) so n_peritonitis_episodes/peritonitis_flag are scoped to
        # the survey period, not just "every episode ever on this catheter"
        t0 = t0,
        t1 = t1
      )
    })
  }


  # Build patient objects, nesting catheters (which themselves nest their
  # own infections -- there is no separate flat infection_list here anymore)
  patient_list <- purrr::map(raw_patients$patient_id, function(pid) {
    row <- raw_patients[raw_patients$patient_id == pid, ]

    pd_patient(
      patient_id = row$patient_id,
      catheters = build_patient_catheters(pid),
      t0 = t0,
      t1 = t1
      # demographics are parked until the demographics list is added back to
      # pd_patient() -- the raw columns are already selected in raw_patients:
      #   date_of_birth, gender, ethnicity, primary_kidney_disease_code,
      #   height, weight, cigarette_smoking_status, diabetes_type,
      #   date_of_death, cause_of_death
      #
      # TODO: transfer_reason/transfer_date (tau -- death, transplant, or
      # permanent HD transfer) also need mapping from the A3 columns
      # (date_of_death/cause_of_death/modality_change_reason_code) so that
      # patient-level censoring is applied to exposure days.
    )
  })
  names(patient_list) <- raw_patients$patient_id


  # Unit-level numbers

  # Total PD cohort
  n_patients <- nrow(raw_patients)

  # Count incident ("new") patients -- those whose earliest catheter start date
  # falls inside [t0, t1]. Each pd_patient already derived this as
  # new_patient_flag via is_incident_patient(), so summing the flags keeps a
  # single definition of "incident" rather than recomputing it from the raw
  # table. A patient with no usable start dates has flag NA and is not counted.
  n_new <- sum(
    vapply(patient_list, function(p) isTRUE(p$new_patient_flag), logical(1))
  )

  # Total patient-years at risk
  tpyar <- total_patient_years(raw_catheters)


  x <- new_pd_unit(
    unit_id = unit_id,
    t0 = t0,
    t1 = t1,
    n_new = as.integer(n_new),
    n_patients = as.integer(n_patients),
    tpyar = tpyar,
    patients = raw_patients,
    catheters = raw_catheters,
    infections = raw_pe_episodes,
    patient_list = patient_list
  )

  validate_pd_unit(x)
}

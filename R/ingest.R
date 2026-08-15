#' Reads raw files and builds the objects
#'
#'
#'
#'

pd_unit <- function(unit_data_path,
                    infection_data_path,
                    t0,
                    t1,
                    unit_id = NA_character_) {

  # Read raw files
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
    dplyr::distinct(patient_id, .keep_all = TRUE)

  raw_catheters <- raw_a3 |>
    dplyr::select(patient_id, catheter_id, insertion_date, procedure_type,
                  pd_start_date, pd_stop_date, removal_reason,
                  peritonitis_date_first_episode,
                  peritonitis_episodes_count)

  raw_pe <- readxl::read_excel(infection_data_path) |>
    dplyr::select(patient_id, catheter_id, date_of_infection,
                  infection_type_peritonitis, relapse_recurrence_code,
                  organism, regimen_type, drug_position, antibiotic_code,
                  route_of_administration, date_of_last_dose,
                  overnight_hospitalisation, catheter_removed,
                  catheter_removed_date, interim_hd, permanent_hd,
                  first_dialysis_date, last_dialysis_date)


}

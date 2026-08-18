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
                  relapse_recurrence_code, organism, last_dose_antibiotic,
                  overnight_hospitalisation, days_hospitalised,
                  catheter_removed, catheter_removed_date,
                  interim_hd, permanent_hd, first_dialysis_date, last_dialysis_date)

  # build nested objects
  infection_object <- raw_infections |>
    dplyr::group_by(c(patient_id, date_of_infection)) |>
    purrr::map()

}

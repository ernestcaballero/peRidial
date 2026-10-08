a3_spec <- list(
  patient_id = list(
    all   = "patient",
    any   = c("id", "identifier", "nhi"),     # patient_id, patient_nhi, patient_identifier
    exact = c("id", "identifier", "nhi")      # a column called just "NHI" or "ID"
  ),
  date_of_birth = list(
    any   = c("birth", "dob"),                # date_of_birth, birth_date, DOB
    exact = "dob"
  ),
  gender = list(
    any   = c("gender", "sex"),               # gender, gender_code, sex
    exact = c("gender", "sex")
  ),
  ethnicity = list(
    any = c("ethnic", "race", "racial")       # ethnicity, ethnicity1_code, racial_origin_code
  ),
  primary_kidney_disease = list(
    all = "primary",
    any = c("kidney", "renal", "disease")     # primary_kidney_disease, primary_renal_disease_code
  ),
  height = list(any = "height"),
  weight = list(any = "weight"),
  cigarette_smoking_status = list(
    any = c("smok", "cigarette")              # cigarette_smoking_status, smoking_code
  ),
  diabetes_type = list(any = "diabet"),

  # catheter / PD course
  insertion_date = list(all = c("insert", "date")),
  procedure_type = list(all = c("procedure")),
  pd_start_date = list(all = c("pd", "start", "date")),
  pd_stop_date = list(all = c("pd", "stop", "date")),
  removal_reason = list(all = c("remov", "reason")),

  # modality change: specific first (all three share "modality")
  modality_change_reason = list(all = c("modality", "reason")),
  date_modality_change = list(all = c("modality", "date")),
  dialysis_modality_change = list(all = c("modality", "change")),
  dialysis_type = list(all = c("dialysis", "type")),

  # outcomes
  date_of_death = list(all = c("death", "date")),
  cause_of_death = list(all = "death", any = c("cause", "reason")),
  transplant_date = list(all = c("transplant", "date"))
)

pe_spec <- list(
  patient_id = a3_spec$patient_id,                 # same rule in both files
  date_of_infection = list(all = c("date", "infect")),
  organism = list(all = "organism"),
  last_dose_antibiotic = list(all = "antibiotic"),           # last_dose_antibiotic, final_antibiotic_dose_date
  # hospitalisation: days first (needs a day/night keyword), overnight flag second
  days_hospitalised = list(all = "hospital", any = c("days", "nights")),
  overnight_hospitalisation = list(all = c("overnight", "hospital")),
  catheter_removed_date = list(all = c("catheter", "remov", "date")),  # specific first
  catheter_removed = list(all = c("catheter", "remov")),
  interim_hd = list(all = "interim"),              # interim_hd
  permanent_hd = list(all = "permanent"),
  first_dialysis_date = list(all = c("first", "dialysis")),
  last_dialysis_date = list(all = c("last",  "dialysis"))
)


organism_map <-

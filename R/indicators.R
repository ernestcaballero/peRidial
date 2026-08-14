#' Calculate exposure days between two dates
#'
#'
#'
#'
#'
exposure_days <- function(min_date, max_date) {
  stopifnot(inherits(min_date, "Date"), inherits(max_date, "Date"))
  as.numeric(pmax(0, max_date - min_date))
}


#' Calculate one patient's exposure in years
#'
#'
#'
#'
#'

exposure_years <- function(pd_start, pd_stop) {
  sum(exposure_days(pd_start, pd_stop)) / 365.25
}



#' Calculate total patient-years-at-risk for a unit
#'
#'
#'
#'
#'
#'


total_patient_years <- function(catheters) {
  stopifnot(is.data.frame(catheters))
  stopifnot(all(c("nhi", "pd_start_date", "pd_stop_date") %in% names(catheters)))

  # calculate exposure years by patient and returns total patient-years-at-risk of the unit
  ey_by_patient <- tapply(
    seq_len(nrow(catheters)),
    catheters$nhi,
    # uses the row indices to pull out that patient's pd_start and pd_stop values
    # and calculate exposure_years
    function(rows) exposure_years(catheters$pd_start[rows], catheters$pd_stop[rows])
  )
  sum(ey_by_patient)
}

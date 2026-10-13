
# These are the helper functions for calculating tau (censoring event of PD like death, transplantation,
# permanent transfer to HD, etc)


#' Derive a patient's censoring point (\eqn{tau})
#'
#' \eqn{tau} is the date a patient permanently stopped being at risk of PD
#' peritonitis. Four events end PD, and the earliest of them wins:
#'
#' \itemize{
#'   \item \strong{Death}: \code{date_of_death} on the A3 form.
#'   \item \strong{Transplant}: see \code{find_transplant_date()}.
#'   \item \strong{Permanent transfer to HD}: of the four
#'     \code{dialysis_modality_change} values only "Any PD to HD" takes a
#'     patient off PD; CAPD<->APD swaps stay within PD and "HD to any PD" is a
#'     return to it.
#'   \item \strong{PD stopped with no successor catheter}: every catheter
#'     has closed and none reopened, so the patient is off PD even though no
#'     explicit modality-change row says so. Controlled by
#'     \code{censor_on_last_stop}, since a patient on a genuine break from PD
#'     looks identical in the data.
#' }
#'
#' @param demo One-row data frame of this patient's demographic fields.
#' @param mod Data frame of this patient's modality-change rows.
#' @param cath Data frame of this patient's catheter rows.
#' @param censor_on_last_stop Logical. Apply the fourth rule above.
#'
#' @return A list with \code{reason} (character), \code{date} (Date),
#'   \code{detail} (character), \code{transplant_gap} (logical: \code{TRUE}
#'   when a "transplant" modality change was recorded but no date could be
#'   found for it) and \code{hd_transfer_gap} (logical: \code{TRUE}
#'   when an "Any PD to HD" modality change was recorded but those
#'   rows had no \code{date_modality_change}).
#' @noRd

PD_TO_HD <- "any pd to hd"      # a modality-change value that takes a patient off PD

derive_patient_tau <- function(demo, mod, cath, censor_on_last_stop = TRUE) {
  dates <- as.Date(character(0))
  reasons <- character(0)
  details <- character(0)

  # 1. death: date from the A3 form
  if ("date_of_death" %in% names(demo) && nrow(demo) > 0 &&
      !is.na(demo$date_of_death[1])) {
    dates <- c(dates, demo$date_of_death[1])
    reasons <- c(reasons, "death")
    detail <- if ("cause_of_death" %in% names(demo)) {
      as.character(demo$cause_of_death[1])
    } else {
      NA_character_
    }
    details <- c(details, detail)
  }

  # 2. transplant: date found by find_transplant_date()
  tx <- find_transplant_date(demo, cath)
  if (!is.na(tx)) {
    dates <- c(dates, tx)
    reasons <- c(reasons, "transplant")
    details <- c(details, NA_character_)
  }

  # a "transplant" modality row with no usable date is flagged, so the caller can warn
  transplant_row <- nrow(mod) > 0 &&
    any(tolower(trimws(as.character(mod$dialysis_modality_change))) == "transplant")
  transplant_gap <- transplant_row && is.na(tx)

  # 3. permanent transfer to HD
  hd_transfer_gap <- FALSE
  if (nrow(mod) > 0) {
    hd_rows <- which(tolower(trimws(as.character(mod$dialysis_modality_change))) == PD_TO_HD)
    to_hd <- hd_rows[!is.na(mod$date_modality_change[hd_rows])]     # only those with a date
    if (length(to_hd) > 0) {
      last_hd <- to_hd[which.max(mod$date_modality_change[to_hd])]  # latest one: a patient may move PD -> HD -> PD -> HD
      dates <- c(dates, mod$date_modality_change[last_hd])
      reasons <- c(reasons, "permanent transfer to HD")
      details <- c(details, trimws(as.character(mod$modality_change_reason[last_hd])))
    } else if (length(hd_rows) > 0) {
      hd_transfer_gap <- TRUE      # PD-to-HD recorded but with no date: flag for a warning
    }
  }

  # if any of rules 1-3 is satisfied, the earliest event is tau
  if (length(dates) > 0) {
    i <- which.min(dates)
    return(list(reason = reasons[i], date = dates[i], detail = details[i],
                transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap))
  }

  # 4. fallback: every catheter has a stop date, so the patient is off PD
  # Optional, since a genuine break from PD looks identical in the data
  if (censor_on_last_stop && nrow(cath) > 0 && !anyNA(cath$pd_stop_date)) {
    last <- which.max(cath$pd_stop_date)      # the last catheter to close
    detail <- if ("removal_reason" %in% names(cath)) {
      as.character(cath$removal_reason[last])
    } else {
      NA_character_
    }
    return(list(reason = "pd stopped",
                date = cath$pd_stop_date[last],
                detail = detail,
                transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap))
  }
  # no censoring event: patient is treated as still on PD
  list(reason = NA_character_, date = as.Date(NA), detail = NA_character_,
       transplant_gap = transplant_gap, hd_transfer_gap = hd_transfer_gap)
}




#' Transplant date for a patient: the recorded date if there is one,
#' or inferred from a catheter removed because of transplant
#' @noRd
find_transplant_date <- function(demo, cath) {
  if (nrow(demo) > 0 && !is.na(demo$transplant_date[1])) {            # use the explicit date
    return(as_date_safe(demo$transplant_date[1], "transplant_date"))
  }
  # fallback: a catheter whose removal reason mentions a transplant
  if ("removal_reason" %in% names(cath) && nrow(cath) > 0) {
    hit <- grepl("transplant", tolower(as.character(cath$removal_reason)))    # matches "Transplant", "transplanted"...
    hit <- hit & !is.na(cath$pd_stop_date)         # needs a stop date to use as the date
    if (any(hit)) {
      return(min(cath$pd_stop_date[hit]))          # earliest such stop date
    }
  }
  as.Date(NA)
}



#' Was the patient on PD at any point in reporting period (t0, t1)?
#' @noRd
on_pd_in_period <- function(cath, t0, t1, tau = as.Date(NA)) {
  if (nrow(cath) == 0) {      # no catheters, never on PD
    return(FALSE)
  }
  starts <- cath$pd_start_date
  stops <- cath$pd_stop_date
  if (!is.na(tau)) {
    stops[is.na(stops) | stops > tau] <- tau        # censoring stops PD
  }
  stops[is.na(stops)] <- t1                         # still active at the end of the period
  any(!is.na(starts) & starts <= t1 & stops >= t0)  # any catheter window overlaps [t0, t1]
}


#' Create a pd_unit object
#'
#' Wraps a tibble/data frame with metadata about the name of PD unit,
#' the reporting period, etc
#'
#' @param data A tibble or dataframe.
#' @param unit Character. The name of PD Unit (e.g. "XYZ PD Unit")
#' @param period_start Date. Start date of reporting period.
#' @param period_end Date. End date of reporting period.
#'
#' @return An object of class \code{pd_unit}.
#' @export

new_pd_unit <- function(data = tibble::tibble(data),
                        unit = NA_character_,
                        period_start = as.Date(NA),
                        period_end = as.Date(NA)) {
  stopifnot(is.data.frame(data))
  structure(
    data,
    class = c("pd_unit", class(data)),
    unit = unit,
    period_start = period_start,
    period_end = period_end
  )
}


#' Create pd_unit object from an Excel file
#'
#' Reads a sheet from an Excel workbook and wraps it
#' as a \code{pd_unit}.

load_new_pd_unit <- function(path,
                             sheet = 1,
                             unit = NA_character_,
                             period_start = as.Date(NA),
                             period_end = as.Date(NA),
                             ...) {
  raw <- readxl::read_excel(path, sheet = sheet, ...)
  new_pd_unit(data = raw,
              unit = unit,
              period_start = period_start,
              period_end = period_end)
}

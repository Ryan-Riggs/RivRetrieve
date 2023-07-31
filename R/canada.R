#' @title canada
#' @name canada
#'
#' @description Retrieve Canadian gauge data
#'
#' @param site Canadian gauge number
#' @param variable Character. Either `stage` or `discharge`.
#' @param start_date Character. Optional start date with format
#'   YYYY-MM-DD. Default is 1900-01-01.
#' @param end_date Character. End date with format YYYY-MM-DD.
#'   Default is the current date.
#' @param sites Logical. If TRUE, returns a list of measurement
#'   sites.
#' @param ... Additional arguments. None implemented.
#'
#' @return data frame of discharge time-series
#' @examples
#' \dontrun{
#' df = canada("01AD003")
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
canada <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {

  if (sites) {
    return(canada_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  if (variable == "discharge") {
    original_data <- hy_daily_flows(site)
  } else if (variable == "stage") {
    original_data <- hy_daily_levels(site)
  }
  original_data <- as_tibble(original_data)
  data <- original_data %>%
    dplyr::select(all_of(c("Date", "Value"))) %>%
    rename(!!column_name := "Value") %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

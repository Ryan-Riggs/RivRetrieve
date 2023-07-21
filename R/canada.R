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
  if (is.null(start_date)) {
    start_date <- "1900-01-01"
  }
  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")
  if (variable == "discharge") {
    original_data <- hy_daily_flows(site)
    colnm <- "Q"
  } else if (variable == "stage") {
    original_data <- hy_daily_levels(site)
    colnm <- "H"
  }
  data <- original_data %>%
    dplyr::select(all_of(c("Date", "Value"))) %>%
    rename(!!colnm := "Value") %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

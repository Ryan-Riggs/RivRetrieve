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
                   ...) {

  if (is.null(start_date))
    start_date <- "1900-01-01"

  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  if (variable == "discharge") {
    data <- try(hy_daily_flows(site))
  } else if (variable == "stage") {
    data <- try(hy_daily_levels(site))
  }
  if (inherits(data, "try-error")) {
    return(NA)
  }
  data <- data %>%
    dplyr::select(all_of(c("Date", "Value"))) %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  if (variable == "discharge") {
    data <- data %>% rename(Q = "Value")
  } else if (variable == "stage") {
    data <- data %>% rename(H = "Value")
  }
  return(data)
}

#' @title usa
#' @name usa
#'
#' @description Retrieve USA gauge data
#'
#' @param site USA gauge number
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
#' \donttest{
#' df <- usa("02471078", variable="discharge")
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
usa <- function(site,
                variable = "stage",
                start_date = NULL,
                end_date = NULL,
                sites = FALSE,
                ...) {

  if (sites) {
    return(usa_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  if (variable == "stage") {
    param_code <- "00065"
    mult <- 0.3048 # ft -> m
  } else if (variable == "discharge") {
    param_code <- "00060"
    mult <- 0.02832 # ft3/s -> m3/s
  }
  original_data <- try(readNWISdv(
    site, param_code, start_date, end_date
  ),silent=TRUE)
  if(is.error(original_data)==TRUE|length(original_data)==0){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  original_data <- as_tibble(original_data)
  data <- original_data %>%
    dplyr::select(3, 4) %>%
    setNames(c("Date", "X")) %>%
    mutate(X = as.numeric(.data$X) * mult) %>%
    rename(!!column_name := "X")
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

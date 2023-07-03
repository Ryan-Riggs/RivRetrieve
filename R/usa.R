#' @title usa
#' @name usa
#'
#' @description Provides access to USA gauge data
#'
#' @param site US gauge number
#' @param variable Character. Either `stage` or `discharge`.
#' @param start_date Character. Optional start date with format
#'   YYYY-MM-DD. Default is 1900-01-01.
#' @param end_date Character. End date with format YYYY-MM-DD.
#'   Default is the current date.
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import dataRetrieval
#' @import data.table
#' @import datasets
#' @import dplyr
#' @import BBmisc
#' @examples
#' df = usa("02471078", variable="discharge")
#' plot(df$Date, df$Q, type='l')
#' @export
usa <- function(site,
                variable = "stage",
                start_date = NULL,
                end_date = NULL,
                ...) {

  if (is.null(start_date))
    start_date <- as.Date("1900-01-01")

  ## if `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  if (variable == "stage") {
    param_code <- "00065"
    colnm <- "H"
    mult <- 0.3048 # ft -> m
  } else if (variable == "discharge") {
    param_code <- "00060"
    colnm <- "Q"
    mult <- 0.02832 # ft3/s -> m3/s
  }
  data <- readNWISdv(site, param_code, start_date, end_date)
  data <- data %>%
    dplyr::select(3, 4) %>%
    setNames(c("Date", "X")) %>%
    mutate(X = as.numeric(X) * mult) %>%
    rename(!!colnm := "X") %>%
    as_tibble()
  return(data)
}

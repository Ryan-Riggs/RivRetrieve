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
#' \dontrun{
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
    mutate(X = as.numeric(.data$X) * mult) %>%
    as_tibble()
  data[[colnm]] <- data[["X"]]
  data <- data %>% dplyr::select(-all_of(c("X")))
  return(data)
}

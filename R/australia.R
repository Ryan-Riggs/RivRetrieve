#' @title australia
#' @name australia
#'
#' @description Retrieve Australian gauge data
#'
#' @param site Australian gauge number
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
#' sites <- australia(sites = TRUE)
#' df <- australia(sites$site[1], "stage")
#' plot(df$Timestamp, df$Value, type='l')
#' }
#' @export

australia <- function(site,
                      variable = "discharge",
                      start_date = NULL,
                      end_date = NULL,
                      sites = FALSE,
                      ...) {

  if (sites) {
    return(australia_sites)
  }
  if (is.null(start_date)) {
    start_date <- "1900-01-01"
  }
  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  ## Recode variables from RivRetrieve convention to BoM convention
  if (variable == "stage") {
    variable <- "Water Course Level"
    colnm <- "H"
  } else if (variable == "discharge") {
    variable <- "Water Course Discharge"
    colnm <- "Q"
  } else {
    stop(sprintf("Variable %s is not available", variable))
  }

  ## Retrieve data using `bomWater` package
  original_data <- get_daily(variable, site, start_date, end_date)

  ## Create return object
  original_data <- as_tibble(original_data)
  data <- original_data %>%
    mutate(Date = as.Date(.data$Timestamp)) %>%
    rename(!!colnm := "Value") %>%
    dplyr::select(c("Date", colnm))
  out <- new_tibble(
    data,
    original = original_data,
    ## variable = "stage"
    ## units = "m"
    class = "rr_tbl"
  )
  return(out)
}

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
#' @param ... Additional arguments. None implemented.
#'
#' @return data frame of discharge time-series
#' @examples
#' \dontrun{
#' df <- australia("403213", "stage")
#' plot(df$Timestamp, df$Value, type='l')
#' }
#' @export

australia <- function(site,
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

  ## Recode variables from RivRetrieve convention to BoM convention
  if (variable == "stage") {
    variable <- "Water Course Level"
  } else if (variable == "discharge") {
    variable <- "Water Course Discharge"
  } else {
    stop(sprintf("Variable %s is not available", variable))
  }
  data <- try(get_daily(variable, site, start_date, end_date))
  if (inherits(data, "try-error")) {
    return(NA)
  }
  data <- data %>%
    mutate(Variable = variable, .before = .data$Value) %>%
    mutate(Timestamp = as.Date(.data$Timestamp))
  return(data)
}

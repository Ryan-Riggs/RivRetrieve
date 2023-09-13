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
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)

  ## Recode variables from RivRetrieve convention to BoM convention
  if (variable == "stage") {
    bom_variable <- "Water Course Level"
  } else if (variable == "discharge") {
    bom_variable <- "Water Course Discharge"
  }

  ## Retrieve data using `bomWater` package
  original_data <- get_daily(
    bom_variable, site, start_date, end_date
  )

  ## Create return object
  original_data <- as_tibble(original_data)
  data <- original_data %>%
    mutate(Date = as.Date(.data$Timestamp)) %>%
    dplyr::select(c("Date", "Value")) %>%
    rename(!!column_name := "Value")
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}


get_daily <- function(parameter_type,
                      station_number,
                      start_date,
                      end_date,
                      var,
                      aggregation,
                      tz,
                      return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  # Handle possible formats of var input
  if (missing(var)) {
    if (parameter_type %in% parameters("discrete")) {
      var <- "Total"
    } else {
      var <- "Mean"
    }
  } else {
    var <- stringr::str_to_title(var)
  }
  if (missing(aggregation)) {
    aggregation <- "24HR"
  } else {
    aggregation <- toupper(aggregation)
  }

  ts_name <- paste0("DMQaQc.Merged.Daily", var, ".", aggregation)

  if (parameter_type %in% parameters("continuous")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyMean.24HR",
      "DMQaQc.Merged.DailyMax.24HR",
      "DMQaQc.Merged.DailyMin.24HR"
    )
    if (parameter_type == "Water Course Discharge") {
      valid_daily_ts <- c(
        valid_daily_ts,
        "DMQaQc.Merged.DailyMean.09HR"
      )
    }
  }

  if (parameter_type %in% parameters("discrete")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyTotal.09HR",
      "DMQaQc.Merged.DailyTotal.24HR"
    )
  }

  if (!ts_name %in% valid_daily_ts) {
    stop("Invalid combination of parameter_type, var and aggregation")
  }

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

parameters <- function(pars) {
  continuous <- c(
    "Dry Air Temperature",
    "Relative Humidity",
    "Wind Speed",
    "Electrical Conductivity At 25C",
    "Turbidity",
    "pH",
    "Water Temperature",
    "Ground Water Level",
    "Water Course Level",
    "Water Course Discharge",
    "Storage Level",
    "Storage Volume"
  )
  discrete <- c(
    "Rainfall",
    "Evaporation"
  )

  if (missing(pars)) {
    return(c(discrete, continuous))
  } else {
    if (!tolower(pars) %in% c("continuous", "discrete")) {
      stop("Invalid parameter category entered")
    }
    return(get(pars))
  }
}

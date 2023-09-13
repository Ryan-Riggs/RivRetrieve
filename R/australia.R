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



get_timeseries <- function(parameter_type,
                           station_number,
                           start_date,
                           end_date,
                           tz,
                           return_fields,
                           ts_name) {

  # If no tz supplied, get from jurisdiction
  if (is.null(tz)) {
    # From BoM website:
    # Which time zones are the data displayed in?
    #   Time of day is presented in local standard time. Coordinated Universal Timezones (UTC) are:
    #
    #   Eastern States (QLD, NSW, ACT, VIC, TAS) - UTC +10:00
    #   Central States (NT, SA) - UTC +09:30
    #   Western Australia - UTC +08:00.

    # Get the station list and custom attributes to determine correct time zone
    station_list <- get_station_list(parameter_type,
                                     station_number,
                                     return_fields="custom_attributes")
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }

    jurisdiction <- stringr::str_split_fixed(station_list$DATA_OWNER_NAME, " -", n = 2)[1]

    # Time zones are selected where there is no DST
    if (jurisdiction %in% c("ACT", "ACTNSW", "NSW", "QLD", "TAS", "VIC")) {
      tz <- "Australia/Queensland" # AEST
    } else if (jurisdiction %in% c("SA", "NT")) {
      tz <- "Australia/Darwin" # ACST
    } else if (jurisdiction == "WA") {
      tz <- "Australia/Perth" # AWST
    } else {
      message("Jurisdiction not found, returning datetimes in UTC")
      tz <- "UTC"
    }
  } else {
    # Check if tz is valid
    if (!tz %in% OlsonNames()) {
      stop("Invalid tz argument. Check it is in OlsonNames().")
    }
    station_list <- get_station_list(parameter_type, station_number)
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }
  }

  # Check string date input is valid
  if (is.character(start_date)) {
    start_date <- lubridate::as_date(start_date, format = "%Y-%m-%d")
  }
  if (is.character(end_date)) {
    end_date <- lubridate::as_date(end_date, format = "%Y-%m-%d")
  }
  if (anyNA(c(start_date, end_date))) {
    stop("Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)")
  }
  # Ensure start is less than end
  if (start_date > end_date) {
    stop("start_date must be less than end_date")
  }

  # Coerce to datetime
  if (lubridate::is.Date(start_date)) {
    start_date <- lubridate::force_tz(lubridate::as_datetime(start_date), tz = tz)
    end_date <- lubridate::force_tz(lubridate::as_datetime(end_date), tz = tz)
  } else if (!lubridate::is.POSIXt(start_date)) {
    stop("Provide dates as a character, date, or datetime object")
  }

  # Fix the offset because BoM expects 00:00 style
  # despite 0000 conforming to ISO8601
  start_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(start_date, usetz = TRUE)
  )
  end_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(end_date, usetz = TRUE)
  )

  # Only accept one station at a time for now
  if (length(station_number) > 1) {
    stop("Only a single station can be requested at a time")
  }

  # If return_fields are missing return Timestamp, Value and Quality Code
  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_id <- get_timeseries_id(
    parameter_type,
    station_number,
    ts_name
  )

  timeseries_values <- get_timeseries_values(
    timeseries_id$ts_id[1],
    start_date,
    end_date,
    return_fields
  )

  # Only process data if it exists
  if (nrow(timeseries_values) > 0) {
    if ("Timestamp" %in% colnames(timeseries_values)) {
      # nolint start
      suppressMessages({
        timeseries_values$Timestamp <- lubridate::as_datetime(timeseries_values$Timestamp, tz = tz)
      })
      timeseries_values <- dplyr::mutate_at(timeseries_values,
                                            dplyr::vars(-"Timestamp"),
                                            utils::type.convert,
                                            as.is = TRUE
      )
      # nolint end
    } else {
      timeseries_values <- dplyr::mutate_all(timeseries_values,
                                             utils::type.convert,
                                             as.is = TRUE
      )
    }
  }
  return(timeseries_values)
}


get_as_stored <- function(parameter_type,
                          station_number,
                          start_date,
                          end_date,
                          tz,
                          return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
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
    "DMQaQc.Merged.AsStored.1"
  )

  return(timeseries_values)
}


get_hourly <- function(parameter_type,
                       station_number,
                       start_date,
                       end_date,
                       tz,
                       return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]

  if (!parameter_type %in% c(
    "Water Course Discharge",
    "Water Course Level",
    "Storage Level",
    "Storage Volume"
  )) {
    stop(
      paste("Hourly data is not available for parameter_type", parameter_type)
    )
  }

  if (missing(tz)) tz <- NULL
  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code", "Interpolation Type")
  }

  timeseries_values <- get_timeseries(
    parameter_type,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    "DMQaQc.Merged.HourlyMean.HR"
  )

  return(timeseries_values)
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


get_monthly <- function(parameter_type,
                        station_number,
                        start_date,
                        end_date,
                        tz,
                        return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  if (parameter_type %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.MonthlyMean.CalMonth"
  }

  if (parameter_type %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.MonthlyTotal.CalMonth")
  }

  if (!exists("ts_name")) stop("Invalid parameter_type")

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


get_yearly <- function(parameter_type,
                       station_number,
                       start_date,
                       end_date,
                       tz,
                       return_fields) {
  parameter_type <-
    parameters()[tolower(parameter_type) == tolower(parameters())]
  if (length(parameter_type) == 0) {
    stop("Invalid parameter requested")
  }

  start_date <- paste0(stringr::str_sub(start_date, 1, 4), "-01-01")
  end_date <- paste0(stringr::str_sub(end_date, 1, 4), "-12-31")

  if (parameter_type %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.YearlyMean.CalYear"
  }

  if (parameter_type %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.YearlyTotal.CalYear")
  }

  if (!exists("ts_name")) stop("Invalid parameter_type")

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

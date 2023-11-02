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
#' \donttest{
#' sites <- australia(sites = TRUE)
#' df <- australia(sites$site[1], "stage")
#' plot(df$Date, df$H, type='l')
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


#' @title Query the BoM WISKI API
#' @description
#' This function queries the Bureau of Meteorology Water Data KISTERS API.
#' A parameter list is passed to make request and the JSON return is parsed
#' depending on what is requested. This function can be used if you want to
#' build your own JSON queries.
#' @param params A named list of parameters.
#' @return
#' A tibble is returned with the columns depending on the request. For
#' \code{get_timeseries} requests, a tibble with zero rows is returned
#' if there is no data available for that query.
make_bom_request <- function(params) {
  bom_url <- "http://www.bom.gov.au/waterdata/services"

  base_params <- list(
    "service" = "kisters",
    "type" = "QueryServices",
    "format" = "json"
  )

  r <- tryCatch(
    {
      r <- httr::RETRY("GET", bom_url, query = c(base_params, params), times = 5, quiet = TRUE)
      httr::stop_for_status(r, task = "request water data from BoM")
      httr::warn_for_status(r, task = "request water data from BoM")
    },
    error = function(e) {
      message(strwrap(
        prefix = " ", initial = "",
        "Request for water data failed. Check your request and make sure
         http://www.bom.gov.au/waterdata/ is online"
      ))
      message("Error message:")
      message(e$message)
    },
    warning = function(w) {
      message("Request for water data raised a warning. Warning message:")
      message(w$message)
    }
  )
  json <- jsonlite::fromJSON(httr::content(r, "text"))

  if (params$request %in% c(
    "getParameterList",
    "getSiteList",
    "getStationList",
    "getTimeseriesList"
  )) {
    if (json[1] == "No matches.") {
      stop("No parameter type and station number match found")
    }
    colnames(json) <- json[1, ]
    tbl <- dplyr::slice(tibble::as_tibble(json), -1)
  } else if (params$request == "getTimeseriesValues") {
    column_names <- unlist(stringr::str_split(json$columns, ","))
    if (length(json$data[[1]]) == 0) {
      tbl <- tibble::tibble(
        Timestamp = lubridate::as_datetime(lubridate::ymd()),
        Value = numeric(),
        `Quality Code` = integer()
      )
    } else {
      colnames(json$data[[1]]) <- column_names
      tbl <- tibble::as_tibble(json$data[[1]])
    }
  }
  return(tbl)
}

#' @title Retrieve water observation stations
#' @md
#' @description
#' `get_station_list` queries Water Data Online and returns station details.
#' Queries can be input with the desired `parameter_type` to find all the
#' stations on record. If you already have a vector of station numbers, you can
#' pass the vector to `station_number` and return the details of those
#' stations.
#' `return_fields` can be customised to return various data about the stations.
#'
#' @param parameter_type The parameter for the station request (e.g. Water
#' Course Discharge, Storage Level)
#' @param station_number Optional: a single or multiple vector of AWRC station
#' numbers.
#' @param bbox Optional: a bounding box to get stations in a region of interest.
#' Takes a vector ordered xmin, ymin, xmax, ymax.
#' @param return_fields  Station details to be returned. By default the columns
#' returned are station name, number, ID, latitude and longitude. Can be
#' customised with a vector of parameters.
#' @return
#' With the default return fields, a tibble with columns station_name,
#' station_no, station_id, station_latitude, station_longitude.
#'
#' @details
#' Possible return fields for `get_station_list()` are:
#'
#' * station_name
#' * station_longname
#' * station_no
#' * station_id
#' * station_latitude
#' * station_longitude
#' * station_carteasting
#' * station_cartnorthing
#' * stationparameter_name
#' * station_georefsystem
#' * catchment_no
#' * catchment_id
#' * catchment_name
#' * site_no
#' * site_id
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * object_type
#' * custom_attributes
#'
#'
#' @examples
#' # Get all Water Course Discharge Stations
#' \dontrun{
#' get_station_list()
#' }
#' # Just the details for Cotter River at Gingera
#' \dontrun{
#' get_station_list(station_number = "410730")
#' }
#' # Rainfall stations
#' \dontrun{
#' get_station_list(parameter_type = "Rainfall")
#' }
#' # Vector of return_fields
#' return_fields <- c(
#'   "station_name",
#'   "station_longname",
#'   "station_no",
#'   "station_id",
#'   "station_latitude",
#'   "station_longitude",
#'   "station_carteasting",
#'   "station_cartnorthing",
#'   "stationparameter_name",
#'   "station_georefsystem",
#'   "catchment_no",
#'   "catchment_id",
#'   "catchment_name",
#'   "site_no",
#'   "site_id",
#'   "site_name",
#'   "parametertype_id",
#'   "parametertype_name",
#'   "object_type",
#'   "custom_attributes"
#' )
#' # Get all attributes for one station
#' \dontrun{
#' get_station_list("Water Course Discharge", "410730", return_fields)
#' }
#' @export
get_station_list <- function(parameter_type, station_number, bbox, return_fields) {
  params <- list("request" = "getStationList")

  # Set default to return all Water Course Discharge stations
  if (missing(parameter_type)) {
    parameter_type <- "Water Course Discharge"
  }
  params[["parameterType_name"]] <- parameter_type

  if (!missing(station_number)) {
    # Support multiple stations
    station_number <- paste(station_number, collapse = ",")
    params[["station_no"]] <- station_number
  }

  if (!missing(bbox)) {
    bbox <- paste(bbox, collapse = ",")
    params[['bbox']] = bbox
  }

  # Set the default return fields
  if (missing(return_fields)) {
    params[["returnfields"]] <- paste(c(
      "station_name",
      "station_no",
      "station_id",
      "station_latitude",
      "station_longitude"
    ),
    collapse = ","
    )
  } else {
    params[["returnfields"]] <- paste(return_fields, collapse = ",")
  }

  get_bom_request <- make_bom_request(params)

  # Convert types
  station_list <- dplyr::mutate_all(
    get_bom_request,
    utils::type.convert,
    as.is = TRUE
  )

  return(station_list)
}

#' @title Retrieve the timeseries ID
#' @description
#' `get_timeseries_id` retrieves the timeseries ID that can be used to obtain
#' values for a parameter type, station and timeseries combination.
#' @param parameter_type The parameter of interest (e.g. Water Course
#' Discharge).
#' @param station_number The AWRC station number.
#' @param ts_name The BoM time series name (e.g. DMQaQc.Merged.DailyMean.24HR).
#' @return
#' Returns a tibble with columns station_name, station_no, station_id, ts_id,
#' ts_name, parametertype_id, parametertype_name.
get_timeseries_id <- function(parameter_type, station_number, ts_name) {
  params <- list(
    "request" = "getTimeseriesList",
    "parametertype_name" = parameter_type,
    "ts_name" = ts_name,
    "station_no" = station_number
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
}

#' @title Retrieve timeseries values
#' @description
#' `get_timeseries_values` returns the timeseries values between a start and end
#' date for given timeseries ID.
#' @param ts_id The timeseries ID for the values of interest. Can be found using
#' the function `get_timeseries_id`.
#' @param start_date The start date formatted as 'YYYY-MM-DD'.
#' @param end_date The end date formatted as 'YYYY-MM-DD'.
#' @param return_fields A vector of the variables that are to be returned.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
get_timeseries_values <- function(ts_id, start_date, end_date, return_fields) {
  params <- list(
    "request" = "getTimeseriesValues",
    "ts_id" = ts_id,
    "from" = start_date,
    "to" = end_date,
    "returnfields" = paste(return_fields, collapse = ",")
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
}

#' @title Retrieve available parameters for stations
#' @md
#' @description
#' `get_parameter_list` returns the parameters that can be retrieved at one or
#' more stations.
#' @param station_number A single or multiple vector of AWRC station
#' numbers.
#' @param return_fields (Optional) Station parameter details to be returned.
#' By default the return fields are: station_no, station_id, station_name,
#' parametertype_id, parametertype_name, parametertype_unitname
#' parametertype_shortunitname.
#' @details
#' The default return fields have been selected to generally return the most
#' useful fields while reducing duplication of metadata.
#' The full list of return fields:
#' * station_no',
#' * station_id
#' * station_name
#' * stationparameter_id
#' * stationparameter_no
#' * stationparameter_name
#' * stationparameter_longname
#' * site_id
#' * site_no
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * parametertype_longname
#' * parametertype_unitname
#' * parametertype_shortunitname
#' @return
#' A tibble with columns for each of the return fields.
#' @examples
#' # Return parameters for a single station
#' \dontrun{
#' get_parameter_list(station_number = "410730")
#' }
#' # Return available parameters for multiple stations
#' \dontrun{
#' get_parameter_list(station_number = c("410730", "570946"))
#' }
#' @export
get_parameter_list <- function(station_number, return_fields) {
  params <- list("request" = "getParameterList")

  if (!missing(station_number)) {
    # Support multiple stations
    station_number <- paste(station_number, collapse = ",")
    params[["station_no"]] <- station_number
  } else {
    stop("No station number provided")
  }

  # Set the default return fields
  if (missing(return_fields)) {
    params[["returnfields"]] <- paste(c(
      "station_no",
      "station_id",
      "station_name",
      "parametertype_id",
      "parametertype_name",
      "parametertype_unitname",
      "parametertype_shortunitname"
    ),
    collapse = ","
    )
  }

  get_bom_request <- make_bom_request(params)

  # Convert types
  parameter_list <- dplyr::mutate_all(
    get_bom_request,
    utils::type.convert,
    as.is = TRUE
  )

  return(parameter_list)
}





#' @title Get time series
#' @md
#' @description Get timeseries data from Water Data online
#' @details This function can be used if you want to retrieve a specific
#' timeseries that is not the default quality checked one.
#'
#' Common valid return fields are:
#'
#' * Timestamp
#' * Value
#' * Quality Code
#' * Interpolation Type
#'
#' Other valid return fields (depending on the parameter requested) may be:
#'
#' * Absolute Value
#' * AV Interpolation
#' * AV Quality Code
#' * Runoff Value
#' * RV Interpolation
#' * RV Quality Code
#' * Aggregation
#' * Accuracy
#'
#' If the request is not valid it will fail.
#' @param parameter_type The water data parameter type (e.g. Water Course
#' Discharge). See \code{\link{parameters()}} for a full list.
#' @param station_number The AWRC station number.
#' @param start_date Start date formatted as a string or date class
#' (YYYY-MM-DD).
#' @param end_date End date formatted as a string or date class (YYYY-MM-DD).
#' @param tz Optional: the desired time zone for the output timeseries. Input
#' must be an Olson Name (see `OlsonNames()`). By default the timeseries are
#' returned in non-DST time zones (AEST, ACST or AWST) depending on the
#' station location.
#' @param return_fields Optional: columns to be returned from Water Data Online.
#' By default Timestamp, Value and Quality Code are returned.
#' @param ts_name The timeseries name (e.g. DMQaQc.Merged.DailyMean.24HR) that
#' is desired.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
#' @examples
#' # Accessible dam storage, as shown on the BoM Water Storage dashboard
#' \dontrun{
#' get_timeseries(
#'   parameter_type = "Storage Volume",
#'   "G8150011",
#'   "2020-01-01",
#'   "2020-01-31",
#'   ts_name = "PR02AVQaQc.Merged.DailyMean.24HR",
#'   tz = NULL,
#'   return_fields = c("Timestamp", "Value", "Quality Code")
#' )
#' }
#' @export

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

#' @title get_as_stored
#' @name get_as_stored
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param parameter_type Parameter value
#' @param station_number Station number
#' @param tz TZ
#' @param return_fields Return fields
#' @return A tibble with the requested return fields. Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#' @examples
#' # Groundwater level as stored by the BoM
#' # PLUMB RD @ NARRABRI'
#' \dontrun{
#' get_as_stored(
#'   parameter_type = "Ground Water Level",
#'   station_number = "GW971623.3.3",
#'   start_date = "2020-03-01",
#'   end_date = "2020-03-01"
#' )
#' }
#' @export
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

#' @title get_hourly
#' @name get_hourly
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param parameter_type Parameter value
#' @param station_number Station number
#' @param tz TZ
#' @param return_fields Return fields
#' @return A tibble with the requested return fields. Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#' @examples
#' # Hourly streamflow Cotter River at Gingera Gauge
#' \dontrun{
#' get_hourly(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31"
#' )
#' }
#' @export
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

#' @title get_daily
#' @name get_daily
#' @param var The daily variable of interest. Valid inputs are `mean`, `min`,
#' `max` for continuous series such as discharge and `total` for discrete
#' series such as rainfall and evaporation.
#' @param aggregation Whether the data is to be aggregated midnight to
#' midnight (`24HR`) or from 9am-9am (`09HR`). The default is `24HR`. `09HR`
#' is only available for mean discharge and total rainfall and evaporation.
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param parameter_type Parameter value
#' @param station_number Station number
#' @param tz TZ
#' @param return_fields Return fields
#' @return A tibble with the requested return fields. Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#' @examples
#' # Download daily mean aggregated over the standard day
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "24HR"
#' )
#' }
#'
#' # Download daily mean aggregated between 9am to 9am
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "09HR"
#' )
#' }
#'
#' # Download the daily max over the standard day
#' \dontrun{
#' get_daily(
#'   parameter_type = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "max",
#'   aggregation = "24HR"
#' )
#' }
#'
#' @export
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

#' @title get_monthly
#' @name get_monthly
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param parameter_type Parameter value
#' @param station_number Station number
#' @param tz TZ
#' @param return_fields Return fields
#' @return A tibble with the requested return fields. Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#' @examples
#' # Monthly average dry air temperature at Corin Dam
#' \dontrun{
#' get_monthly(
#'   parameter_type = "Dry Air Temperature",
#'   station_number = "570947",
#'   start_date = "2016-01-01",
#'   end_date = "2016-06-01"
#' )
#' }
#' @export
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

#' @title get_yearly
#' @name get_yearly
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @param parameter_type Parameter value
#' @param station_number Station number
#' @param tz TZ
#' @param return_fields Return fields
#' @return A tibble with the requested return fields. Zero row tibbles are returned if no data is available for the requested dates. The aggregation of data is generally the mean for most variables, except for rainfall and evaporation which is the sum over the chosen period.
#' @examples
#' # Download annual rainfall for Cotter Hut
#' \dontrun{
#' get_yearly(
#'   parameter_type = "Rainfall",
#'   station_number = "570946",
#'   start_date = 2016,
#'   end_date = 2020
#' )
#' }
#'
#' @export
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


#' @title Available water parameters
#' @aliases parameters()
#' @description
#' `parameters` returns a vector of parameters that can be retrieved from
#' Water Data Online.
#' @param pars Optional: if empty all available parameters will be returned.
#' Alternatively, a vector of the continuous or discrete parameters can be
#' requested.
#' @return
#' A vector of parameters.
#' @examples
#' parameters()
#' parameters("continuous")
#' parameters("discrete")
#' @export
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

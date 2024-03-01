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

  ## Retrieve 'timeseries_id' using the bom package.
  id = get_timeseries_id(bom_variable, site,'DMQaQc.Merged.DailyMean.24HR')

  ##Interface with the BoM website.
  web = 'http://www.bom.gov.au/waterdata/services?service=kisters&type=queryServices&request=getTimeseriesValues&datasource=0&format=csv&ts_id='
  website = paste0(web, id$ts_id, '&from=', start_date, 'T00:00:00.000&to=', end_date,'T00:00:00.000&returnfields=Timestamp,Value,Quality%20Code,Interpolation%20Type&language=en&downloadaszip=false&timezone=individual&csvdiv=%2C&md_returnfields=station_longname,station_no,station_latitude,station_longitude,parametertype_name,ts_name,ts_unitname,custom_attributes&custattr_returnfields=DATA_OWNER_NAME&metadata=true&downloadfilename=csv.australia_file')
  out = tempfile()
  test = download.file(website, out, method='curl', quiet=TRUE)
  original_data = read.csv(out, header = TRUE,skip=9)
  if(is.error(original_data)==TRUE){stop('This gauge does not have a record associated with it and/or the agency website is down.')}

  ## Create return object
  original_data <- as_tibble(original_data)
  data <- original_data %>%
    mutate(Date = as.Date(.data$X.Timestamp)) %>%
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


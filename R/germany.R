#' @title germany
#' @name germany
#'
#' @description Retrieve German gauge data.
#'
#' @param site German gauge id.
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
#' #Note the German API currently only provides access to stage data from the previous 30 days, so start and end date is ignored for this function.
#' site <- "593647aa-9fea-43ec-a7d6-6476a76ae868"
#' stage <- germany(site, variable = "stage")
#' plot(stage$Date, stage$H, type = "l")
#' }
#' @export
germany <- function(site,
                     variable = "discharge",
                     start_date = NULL,
                     end_date = NULL,
                     sites = FALSE,
                     ...) {

  if (sites) {
    return(german_sites)
    }

  if (!variable %in% c("stage", "discharge")) {
    stop("variable must be either 'stage' or 'discharge'")
  }
  param_code  <- if (variable == "stage") "W" else "Q"
  column_name <- if (variable == "discharge") "Q" else "H"
  latest_available=Sys.Date()
  end_date <- if (is.null(end_date)) Sys.Date() else as.Date(end_date)

  # --- PEGELONLINE-specific: clamp start_date to the 30-day window --------
  earliest_available <- Sys.Date() - 30
  start_date <- if (is.null(start_date)) earliest_available else as.Date(start_date)
  if (start_date < earliest_available) {
    warning(
      "PEGELONLINE only retains 30 days of raw data. Requested start_date ",
      start_date, " is earlier than that -- clamping to ", earliest_available, "."
    )
    start_date <- earliest_available
  }

  if (end_date < earliest_available|end_date>latest_available) {
    warning(
      "PEGELONLINE only retains 30 days of raw data. Requested end_date ",
      end_date, " is outside of data range -- clamping to ", latest_available, "."
    )
    end_date <- latest_available
  }

  original_data <- try(
    download_germany_data(site, param_code, start_date, end_date),
    silent = TRUE
  )
  if (is.error(original_data) == TRUE || nrow(original_data) == 0) {
    stop(
      "This gauge does not have a record associated with it and/or the ",
      "agency website is down."
    )
  }

  dt <- data.table::as.data.table(original_data)
  dt[, Date := as.Date(timestamp)]
  data.table::setnames(dt, "value", column_name)

  dt <- dt[Date>=start_date&Date<=end_date, .(value = mean(get(column_name), na.rm = TRUE)), by = Date]

  data.table::setnames(dt, "value", column_name)
  data.table::setorder(dt, Date)

  # PEGELONLINE reports stage (W) in centimeters relative to a local gauge
  # datum, converting to meters.
  if (variable == "stage") {
    dt[[column_name]] <- dt[[column_name]] / 100
  }

  out <- new_tibble(
    tibble::as_tibble(dt),
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_germany_data <- function(site, param_code, start_date, end_date) {
  url <- paste0(
    "https://www.pegelonline.wsv.de/webservices/rest-api/v2/stations/",
    site, "/", param_code, "/measurements.json",
    "?start=", format(start_date, "%Y-%m-%d"),
    "&end=", format(end_date, "%Y-%m-%d")
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  httr::stop_for_status(resp, task = "request measurements from PEGELONLINE")
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  if (length(parsed) == 0) {
    return(data.table::data.table(timestamp = character(), value = numeric()))
  }
  data.table::as.data.table(parsed)
}


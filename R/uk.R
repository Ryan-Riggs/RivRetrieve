#' @title uk
#' @name uk
#'
#' @description Retrieve UK gauge data
#'
#' @param site UK gauge number
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
#' site <- "http://environment.data.gov.uk/hydrology/id/stations/3c5cba29-2321-4289-a1fd-c355e135f4cb"
#' x <- uk(site, variable = "discharge")
#' plot(x$Date, x$Q, type='l')
#' }
#' @export
uk <- function(site,
               variable,
               start_date = NULL,
               end_date = NULL,
               sites = FALSE,
               ...) {

  if (sites) {
    return(uk_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- download_uk_data(
    site, variable, start_date, end_date
  )
  data <- original_data
  ## Aggregate to get daily data
  if (variable == "stage") {
    data <- data %>%
      group_by(date) %>%
      summarize(value = sum(.data$value), count = n()) %>%
      filter(count == 96)
  }
  ## Merge with complete time series in case any missing,
  ## then select columns
  complete_ts <- seq(min(data$date), max(data$date), by = "1 day")
  data <- tibble(date = complete_ts) %>%
    left_join(data, by = "date") %>%
    dplyr::select(all_of(c("date", "value"))) %>%
    rename(Date = "date", !!column_name := "value")
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_uk_data <- function(site, variable, start_date, end_date) {
  if (variable == "stage") {
    ptn <- "level-i-900-m-qualified"
  } else if (variable == "discharge") {
    ptn <- "flow-m-86400-m3s-qualified"
  }
  ## Parse site code and check whether site has data for given variable
  first <- strsplit(site, "/")
  site <- first[[1]][[7]]
  base_url <- "http://environment.data.gov.uk"
  url <- paste0(
    base_url,
    "/hydrology/id/measures?station=", site
  )
  r <- GET(url)
  c <- content(r)
  measures <- sapply(c$items, FUN = function(x) x$notation)
  ix <- grep(ptn, measures)
  if (length(ix) != 1) {
    stop(sprintf("Site %s does not have %s data", site, variable))
  } else {
    notation <- measures[ix]
  }
  ## Get data
  done <- FALSE
  data_list <- list()
  ## API places a hard limit of 2000000 records which is fine for
  ## daily discharge but has to be managed for 15-min stage data.
  while (!done) {
    url <- paste0(
      base_url,
      "/hydrology/id/measures/", notation, "/readings",
      "?mineq-date=", start_date, "&maxeq-date=", end_date,
      "&_limit=2000000"
    )
    r <- GET(url)
    c <- content(r)
    selected_items <- lapply(
      c$items, FUN = function(x) x[c("date", "dateTime", "value", "quality")]
    )
    x <- list.stack(selected_items) %>%
      as_tibble() %>%
      mutate(date = as.Date(.data$date))
    max_date <- max(x$date)
    if ((nrow(x) == 2000000) && (max_date < end_date)) {
      start_date <- max_date
    } else {
      done <- TRUE
    }
    data_list[[length(data_list) + 1]] <- x
  }
  original_data <- do.call("rbind", data_list)
  return(original_data)
}

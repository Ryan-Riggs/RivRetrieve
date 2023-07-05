
base_url <- "http://environment.data.gov.uk"

#' @title uk
#' @name uk
#'
#' @description Provides access to UK gauge data
#'
#' @param site UK gauge number
#' @param variable Character. Either `stage` or `discharge`.
#' @param start_date Character. Optional start date with format
#'   YYYY-MM-DD. Default is 1900-01-01.
#' @param end_date Character. End date with format YYYY-MM-DD.
#'   Default is the current date.
#' @param ... Additional arguments. None implemented.
#'
#' @return data frame of discharge time-series
#' @import httr
#' @import rlist
#' @import dplyr
#' @examples
#' site <- "http://environment.data.gov.uk/hydrology/id/stations/3c5cba29-2321-4289-a1fd-c355e135f4cb"
#' x <- uk(site, variable = "discharge")
#' plot(x$Date, x$Q, type='l')
#' @export
uk <- function(site, variable, start_date = NULL, end_date = NULL) {

  if (is.null(start_date))
    start_date <- as.Date("1900-01-01")

  ## if `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  if (variable == "stage") {
    ptn <- "level-i-900-m-qualified"
    colnm <- "H"
  } else if (variable == "discharge") {
    ptn <- "flow-m-86400-m3s-qualified"
    colnm <- "Q"
  }

  ## Parse site code
  first <- strsplit(site, "/")
  site <- first[[1]][[7]]
  url <- paste0(
    base_url,
    "/hydrology/id/measures?station=", site
  )
  r <- GET(url)
  c <- content(r)
  measures <- sapply(c$items, FUN=function(x) x$notation)
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
    selected_items <- list.select(c$items, date, dateTime, value, quality)
    x <- list.stack(selected_items) %>%
      as_tibble() %>%
      mutate(date = as.Date(date))

    max_date <- max(x$date)
    if (nrow(x) == 2000000 & max_date < end_date) {
      start_date <- max_date
    } else {
      done <- TRUE
    }
    data_list[[length(data_list) + 1]] <- x
  }
  x <- do.call("rbind", data_list)
  ## Aggregate to get daily data
  if (variable == "stage") {
    x <- x %>%
      group_by(date) %>%
      summarize(value = sum(value), count = n()) %>%
      filter(count == 96)
  }

  ## Merge with complete time series in case any missing,
  ## then select columns
  complete_ts <- seq(min(x$date), max(x$date), by = "1 day")
  x <- tibble(date = complete_ts) %>%
    left_join(x, by = "date") %>%
    dplyr::select(date, value) %>%
    rename(Date = date, !!colnm := value)
  x
}

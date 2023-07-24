#' @title japan
#' @name japan
#'
#' @description Retrieve Japanese gauge data
#'
#' @param site Japanese gauge number
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
#' start_date <- as.Date("2019-01-01")
#' end_date <- as.Date("2022-12-31")
#' df <- japan("301011281104010", "discharge", start_date, end_date)
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
japan <- function(site,
                  variable = "discharge",
                  start_date = NULL,
                  end_date = NULL,
                  sites = FALSE,
                  ...) {

  if (sites) {
    return(japan_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- download_japan_data(site, variable, start_date, end_date)
  data <- parse_japan_data(original_data)
  data <- data %>%
    rename(!!column_name := "Value")
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_japan_data <- function(site, variable, start_date, end_date) {
  base_url <- "http://www1.river.go.jp/cgi-bin/DspWaterData.exe"
  ## Divide timeseries into months, because we can only
  ## scrape data one month at a time.
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  ts <- seq.Date(
    floor_date(start_date, "month"),
    floor_date(end_date, "month"),
    by = "1 month"
  )
  dates <- format(ts, format = "%Y%m%d")
  ## Providing ending for URL (arbitrary, so long as further
  ## ahead than one month from the start date)
  ending <- format(ceiling_date(end_date, "month") - days(1), "%Y%m%d")

  ## What should be downloaded?
  if (variable == "stage") {
    kind <- 2
  } else if (variable == "discharge") {
    kind <- 6
  }
  data_list <- list()
  header <- NULL
  for (k in 1:length(dates)){
    day <- dates[k]
    website <- paste0(
      base_url,
      "?KIND=", kind,
      "&ID=", site,
      "&BGNDATE=", day,
      "&ENDDATE=", ending
    )
    file <- html_session(website) %>%
      read_html() %>%
      html_element("body") %>%
      html_table()
    if (is.null(header)) {
      header <- file[1:4, ]
    }
    ## Remove header
    data <- file[5:nrow(file), ]
    data_list[[length(data_list) + 1]] <- data
  }
  ## Join monthly sections together, and filter NA
  data <- do.call("rbind", data_list)
  data <- rbind(header, data)
  return(data)
}

parse_japan_data <- function(data) {
  ## Remove header
  data <- data[5:nrow(data), ]
  ## First column is the time
  dates <- as.Date(data[[1]], format = "%Y/%m/%d")
  ## The other columns are the data, taken at hourly intervals
  data <- data[, 2:ncol(data)]
  data <- data %>%
    mutate(across(everything(), ~na_if(., ""))) %>%
    as_tibble()
  ## We use `as.numeric` to implicitly convert non-numeric values
  ## to NA. This throws a warning which we can safely ignore.
  data <- suppressWarnings(
    data %>% mutate(across(everything(), as.numeric))
  )
  vals <- rowMeans(data, na.rm = TRUE) %>% na_if(NaN)
  data <- tibble(Date = dates, Value = vals) %>%
    filter(!is.na(Value))
  ## Make sure timeseries is complete
  ts <- tibble(Date = seq.Date(start_date, end_date, by = "1 day"))
  data <- ts %>% left_join(data, by = "Date")
  return(data)
}

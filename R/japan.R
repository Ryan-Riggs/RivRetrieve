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

  path <- "http://www1.river.go.jp/cgi-bin/DspWaterData.exe"

  if (is.null(start_date))
    start_date <- "1900-01-01"

  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  ## Divide timeseries into months, because we can only
  ## scrape data one month at a time.
  ts <- seq(
    floor_date(start_date, "month"),
    floor_date(end_date, "month"),
    by = "1 month"
  )
  dates <- format(ts, format = "%Y%m%d")

  ## Providing ending for URL (arbitrary, so long as further ahead than one month from the start date)
  ending <- format(ceiling_date(end_date, "month") - days(1), "%Y%m%d")

  ## What should be downloaded?
  if (variable == "stage") {
    kind <- 2
    colnm <- "H"
  } else if (variable == "discharge") {
    kind <- 6
    colnm <- "Q"
  }

  tab <- list()
  for (k in 1:length(dates)){
    day <- dates[k]
    website <- paste0(
      path,
      "?KIND=", kind,
      "&ID=", site,
      "&BGNDATE=", day,
      "&ENDDATE=", ending
    )
    ## file <- try(
    ##   html_session(website) %>%
    ##   read_html() %>%
    ##   html_element("body")
    ## )
    ## if (inherits(file, "try-error")) {
    ##   next
    ## }
    ## file1 <- try(file %>% html_table())
    ## if (inherits(file1, "try-error")) {
    ##   next
    ## }
    file <- html_session(website) %>%
      read_html() %>%
      html_element("body")
    file1 <- file %>% html_table()
    ## Remove header
    df <- file1[5:nrow(file1), ]
    ## First column is the time
    dts <- as.Date(df[[1]], format = "%Y/%m/%d")
    ## The other columns are the data, taken at hourly intervals
    df <- df[, 2:ncol(df)]
    df <- df %>%
      mutate(across(everything(), ~na_if(., ""))) %>%
      as_tibble()
    ## We use `as.numeric` to implicitly convert non-numeric values
    ## to NA. This throws a warning which we can safely ignore.
    df <- suppressWarnings(
      df %>% mutate(across(everything(), as.numeric))
    )
    vals <- rowMeans(df, na.rm = TRUE) %>% na_if(NaN)
    df <- tibble(Date = dts, Value = vals)
    tab[[k]] <- df
  }
  ## Join monthly sections together, and filter NA
  df <- do.call("rbind", tab) %>%
    filter(!is.na(.data$Value))
  ## Make sure timeseries is complete
  ts <- tibble(Date = seq.Date(start_date, end_date, by = "1 day"))
  df <- ts %>% left_join(df, by = "Date")
  df[[colnm]] <- df[["Value"]]
  df <- df %>% dplyr::select(-all_of(c("Value")))
  return(df)
}

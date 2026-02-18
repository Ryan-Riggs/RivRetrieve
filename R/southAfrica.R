#' @title southAfrica
#' @name southAfrica
#'
#' @description Retrieve South African gauge data
#'
#' @param site South African gauge number
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
#' site <- "X3H023"
#' start_date <- as.Date("2000-01-01")
#' end_date <- as.Date("2010-01-01")
#' x <- southAfrica(site, "stage", start_date, end_date)
#' }
#' @export
southAfrica <- function(site,
                        variable = "stage",
                        start_date = NULL,
                        end_date = NULL,
                        sites = FALSE,
                        ...) {

  if (sites) {
    return(southAfrican_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- try(download_sa_data(
    site, variable, start_date, end_date, primary = FALSE
  ),silent=TRUE)
  if(is.error(original_data)==TRUE|length(original_data)==0){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  data <- original_data %>%
    mutate(DATE = as.Date(.data$DATE, format = "%Y%m%d"))
  if (variable == "stage") {
    data <- data %>%
      mutate(across(starts_with("COR_"), as.numeric)) %>%
      rename(Date = "DATE") %>%
      group_by(.data$Date) %>%
      summarize(!!column_name := mean(.data$COR_LEVEL))
  } else {
    data <- data %>%
      rename(Date = "DATE", !!column_name := "D_AVG_FR") %>%
      dplyr::select(all_of(c("Date", column_name)))
  }
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

construct_endpoint <- function(site, data_type, start_date, end_date) {
  chunk_start_date <- format(start_date, "%Y-%m-%d")
  chunk_end_date <- format(end_date, "%Y-%m-%d")
  endpoint <- paste0(
    "https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?",
    "Station=", site, "100.00",
    "&DataType=", data_type,
    "&StartDT=", chunk_start_date,
    "&EndDT=", chunk_end_date,
    "&SiteType=RIV"
  )
  return(endpoint)
}

download_sa_data <- function(site,
                             variable,
                             start_date,
                             end_date,
                             primary) {
  ## Convert to date
  start_date=as.Date(start_date)
  end_date=as.Date(end_date)

  ## divide timeseries into months, because we can only
  ## scrape data one month at a time.
  ts <- seq(start_date, end_date, by = "1 day")
  years <- year(ts) %>%
    unique() %>%
    sort()
  n_years <- length(years)
  if (primary || (variable == "stage")) {
    data_type <- "Point"
    header <- c(
      "DATE", "TIME", "COR_LEVEL",
      "COR_LEVEL_QUAL", "COR_FLOW", "COR_FLOW_QUAL"
    )
  } else {
    data_type <- "Daily"
    header <- c("DATE", "D_AVG_FR", "QUAL")
  }
  ## Number of data columns
  n_cols <- length(header)
  data_list <- list()
  endpoint = construct_endpoint(site, data_type,start_date,end_date)
    response <- GET(endpoint)
    data <- content(response) %>%
      html_element("body") %>%
      html_text("pre")
    data <- str_split(data, '\n')
    data <- unlist(data)
    ## Find out whether there is any data for
    ## the requested time period
    header_row <- grep("^DATE", data)
    if (length(header_row) == 0) {
      next
    } else {
      header_row <- header_row[1]
    }
    data_rows <- grep("^[0-9]{8} ", data)
    ## Convert to list
    data <- as.list(data)
    ## Get header
    data <- data[data_rows]
    data_sub <- lapply(data, function(x){
      sub <- x %>% str_split(' +')
      sub <- unlist(sub)
      sub
    })
    full_values = lapply(data_sub,length)
    data <- do.call("rbind", data_sub[full_values==3])
    colnames(data) <- header
    data <- data %>% as_tibble()
  original_data <- data
  return(original_data)
}

southAfrica(site='B1H001','discharge')

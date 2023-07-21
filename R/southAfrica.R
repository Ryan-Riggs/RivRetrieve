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
#' @param primary Logical. Whether to return the raw data or
#'   compute daily means. If `variable="discharge"` and `primary=FALSE`
#'   then the function will download the aggregated data directly from
#'   the website. However, if `variable="stage"` and `primary=FALSE` then
#'   the function will first download the primary data from the website and
#'   then compute daily values, because aggregated stage data is not
#'   available from the website. Otherwise if `primary=TRUE` then sub-daily
#'   data for either `stage` or `discharge` will be returned. Note that the
#'   primary data is an irregular time series.
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
#' ## Daily:
#' x <- southAfrica(site, "stage", start_date, end_date)
#' ## Sub-daily:
#' y <- southAfrica(site, "stage", start_date, end_date, primary = TRUE)
#' }
#' @export
southAfrica <- function(site,
                        variable = "stage",
                        start_date = NULL,
                        end_date = NULL,
                        primary = FALSE,
                        sites = FALSE,
                        ...) {

  if (sites) {
    return(southAfrican_sites)
  }

  ## TODO is it somehow possible to retrieve the start and end
  ## point of the data record?
  if (is.null(start_date))
    start_date <- as.Date("1900-01-01")

  ## if `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%y-%m-%d")

  ## divide timeseries into months, because we can only
  ## scrape data one month at a time.
  ts <- seq(start_date, end_date, by = "1 day")
  years <- year(ts) %>% unique() %>% sort()
  n_years <- length(years)
  if (primary || (variable == "stage")) {
    ## We have to download stage data from primary data, which
    ## can only be downloaded one year at a time
    chunk_size <- 1 # Chunk size = num years
    n_chunks <- n_years
    data_type <- "Point"
    header <- c(
      "DATE", "TIME", "COR_LEVEL",
      "COR_LEVEL_QUAL", "COR_FLOW", "COR_FLOW_QUAL"
    )
  } else {
    chunk_size <- 20
    n_chunks <- ceiling(n_years / chunk_size)
    data_type <- "Daily"
    header <- c("DATE", "D_AVG_FR", "QUAL")
  }
  ## Number of data columns
  n_cols <- length(header)

  construct_endpoint <- function(site, data_type, chunk_start_date, chunk_end_date) {
    ## web <- 'https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station='
    ## end <- '100.00&DataType=Daily&StartDT=1900-01-01&EndDT=2023-03-22&SiteType=RIV'
    chunk_start_date <- format(chunk_start_date, "%Y-%m-%d")
    chunk_end_date <- format(chunk_end_date, "%Y-%m-%d")
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

  ## TODO add progress bar?
  data_list <- list()
  ## pb <- txtProgressBar(min=0, max=n_chunks)
  for (i in 1:n_chunks) {
    chunk_start_date <- start_date + years((i-1) * chunk_size)
    endpoint <- construct_endpoint(site, data_type, chunk_start_date, end_date)
    data <- html_session(endpoint) %>%
      html_element('body') %>%
      html_text('pre')
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
      if (length(sub) > n_cols) {
        sub <- sub[1:n_cols]
      } else if (length(sub) < n_cols) {
        sub <- c(sub[1], rep(NA, n_cols - 1))
      }
      sub
    })
    data <- do.call("rbind", data_sub)
    colnames(data) <- header
    data <- data %>% as_tibble()
    data_list[[i]] <- data
    ## setTxtProgressBar(pb, i)
  }
  ## close(pb)
  data <- do.call("rbind", data_list)

  if (primary) {
    data <- data %>%
      unite("time", "DATE", "TIME", sep="") %>%
      mutate(time = as.POSIXct(time, tz="Etc/GMT+2", format="%Y%m%d%H%M%S")) %>%
      mutate(across(starts_with("COR_"), as.numeric))

    if (variable == "stage") {
      data <- data %>%
        rename(stage= "COR_LEVEL", qc = "COR_LEVEL_QUAL") %>%
        dplyr::select(all_of(c("time", "stage", "qc")))
    } else {
      data <- data %>%
        rename(discharge = "COR_FLOW", qc = "COR_FLOW_QUAL") %>%
        dplyr::select(all_of(c("time", "discharge", "qc")))
    }

  } else {
    data <- data %>%
      mutate(DATE = as.Date(.data$DATE, format = "%Y%m%d"))
    if (variable == "stage") {
      data <- data %>%
        mutate(across(starts_with("COR_"), as.numeric)) %>%
        group_by(.data$DATE) %>%
        summarize(
          stage = mean(.data$COR_LEVEL),
          qc = max(.data$COR_LEVEL_QUAL),
          n_obs = n()
        )

    } else {
      data <- data %>%
        rename(date = "DATE", discharge = "D_AVG_FR", qc = "QUAL")
    }
  }
  return(data)
}

## ##Author: Ryan Riggs
## ##Date: 3/24/2023
## #########################################################
## ##########################################################################################################
## ##website
## ##########################################################################################################
## southAfrica=function(f){
##   web='https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station='
##   end='100.00&DataType=Daily&StartDT=1900-01-01&EndDT=2023-03-22&SiteType=RIV'
##   comb=paste0(web, f, end)
##   data=html_session(comb)%>%html_element('body')%>%html_text('pre')
##   data = str_split(data, '\n')
##   data = unlist(data)
##   data = as.list(data)
##   data = data[11:length(data)]
##   data_sub=lapply(data, function(x){
##     sub=x%>%str_split(' +')
##     sub=unlist(sub)
##     output=data.table(t(sub))
##     if(ncol(output)!=3){
##       return(NA)
##     }
##     colnames(output) = c('Date', 'Q', 'Quality')
##     output$Q=as.numeric(output$Q)
##     output$Date = as.Date(output$Date, format='%Y%m%d')
##     return(output)
##   })
##   data_sub = data_sub[!is.na(data_sub)]
##   data_sub = rbindlist(data_sub)
##   return(data_sub)
## }

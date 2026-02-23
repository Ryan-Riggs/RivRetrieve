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
#' start_date <- as.Date("1968-01-01")
#' end_date <- as.Date("1970-12-31")
#' df <- japan("301011281104090", "discharge", start_date, end_date)
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
  data <- try(download_japan_data(site, variable, start_date, end_date),silent=TRUE)
  if(is.error(data)==TRUE){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  if(nrow(data)==0){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  return(data)
}

download_japan_data <- function(site, variable, start_date, end_date) {
  base_url <- "http://www1.river.go.jp/cgi-bin/DspWaterData.exe"
  ## Divide timeseries into years, because we can only
  ## scrape data one year at a time.
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  ts = seq(year(start_date),
                year(end_date))

  ## What should be downloaded?
  if (variable == "stage") {
    kind <- 3
  } else if (variable == "discharge") {
    kind <- 7
  }
  data_list <- list()
  header <- NULL
  output_list=list()
  for (k in 1:length(ts)){
    day <- dates[k]
    website <- paste0(
      base_url,
      "?KIND=", kind,
      "&ID=", site,
      "&BGNDATE=", paste0(ts[k],'0101'),
      "&ENDDATE=", paste0(ts[k],'1231')
    )
    file <- read_html(website,encoding='ISO-8859-1') %>%
      html_element("body") %>%
      html_table()
    if (is.null(header)) {
      header <- file[1:4, ]
    }
    ## Remove header
    data <- file[5:nrow(file), ]
    data <- rbind(header, data)
    ## Remove header
    data <- data[5:nrow(data), ]
    colnames(data)=c('Month',seq(1:31))
    data$Month = gsub("[^0-9.]", "", data$Month)
    data= as_tibble(data)

    tab=list()
    for(i in 1:nrow(data)){
      mnth=data$Month[i]
      days=colnames(data)[2:ncol(data)]
      values=suppressWarnings(as.numeric(data[i,2:ncol(data)]))
      year = ts[k]
      dates=as.Date(paste(year,mnth,days,sep='-'),format='%Y-%m-%d')
      tab[[i]] = data.frame(dates,values)
    }
    combined = do.call("rbind", tab)
    combined = na.omit(combined)
    output_list[[k]]=combined
  }
  combined_output=do.call('rbind',output_list)
  colnames(combined_output) = c('Date',ifelse(variable=='discharge','Q','H'))
  combined_output = new_tibble(combined_output,
                      original=data,
                      class = "rr_tbl")
  combined_output = combined_output[order(combined_output$Date),]
  return(combined_output)
}

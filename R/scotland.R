#' @title scotland
#' @name scotland
#'
#' @description Retrieve Scotland gauge data
#'
#' @param site Scotland gauge number
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
#' df <- scotland("14969", "discharge")
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
scotland <- function(site,
                  variable = "discharge",
                  start_date = NULL,
                  end_date = NULL,
                  sites = FALSE,
                  ...) {

  if (sites) {
    return(scotland_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  data <- try(download_scotland_data(site, variable, start_date, end_date),silent=TRUE)
  if(is.error(data)==TRUE){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  if(nrow(data)==0){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  return(data)
}
# Discharge = Q, Level = SG
download_scotland_data = function(site, variable, start_date = NULL, end_date = NULL) {
  param = ifelse(variable == 'stage', 'SG', 'Q')
  param_output = .get_column_name(variable)
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)

  website = paste0('https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=gettimeseriesvalues&ts_path=1/',
                   site, '/', param, '/HDay.Mean&from=', start_date, '&to=', end_date, '&metadata=true&returnfields=Timestamp,Value,Quality%20Code')

  data_pull = read_html(website) %>% html_table()
  data = data_pull[[1]]
  index = grep('Timestamp', data$X1)
  headers = data[index, ]
  df = as.data.frame(data[(index + 1):nrow(data), ])
  colnames(df) = headers
  df = df[!is.na(df$Timestamp)&df$Timestamp!='',]
  original_data = new_tibble(df)

  modified_data <- df %>% dplyr::select(all_of(c("Timestamp", "Value"))) %>%
    rename(Date = "Timestamp", !!param_output := "Value")
  modified_data$Date = as.Date(modified_data$Date)
  modified_data[, 2] = as.numeric(modified_data[, 2])

  out <- new_tibble(
    modified_data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

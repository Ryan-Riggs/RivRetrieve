#' @title canada
#' @name canada
#'
#' @description Retrieve Canadian gauge data
#'
#' @param site Canadian gauge number
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
#' #Note this no longer relies on tidyhydat but rather retrieves data from an API.
#' df = canada("01AD003")
#' plot(df$Date, df$Q, type='l')
#' }
#' @export

canada <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {
  if (sites) {
    return(canada_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data = try(canada_pull(site,variable,start_date,end_date))
  if (is.error(original_data)) return(stop('This gauge does not have a record associated with it and/or the agency website is down.'))
  original_data <- as_tibble(original_data)
  value_name=colnames(original_data)[grep('Value',colnames(original_data))]
  data <- original_data %>%
    dplyr::select(all_of(c("Date", value_name))) %>%
    rename(!!column_name := value_name) %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

canada_pull=function(site,variable,start_date,end_date){
  param=ifelse(variable=='stage','level','flow')
  url=paste0('https://wateroffice.ec.gc.ca/services/daily_data/csv/inline?stations[]=',site,'&parameters[]=',param,'&start_date=',start_date,'&end_date=',end_date)
  data=content(GET(url))
}


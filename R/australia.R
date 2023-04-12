#' @title australia
#' @name australia
#'
#' @description Provides access to Australian gauge data
#'
#' @param site Australian gauge number
#'
#' @return data frame of discharge time-series
#' @import bomWater
#' @Remote: git::https://github.com/buzacott/bomWater.git
#' @import devtools
#' @import RSelenium
#' @import bomWater
#' @import data.table
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @examples
#' df = australia('403213')
#' plot(df$Date, df$Q, type='l')
#' @export



##Author: Ryan Riggs
##Date: 3/22/2023
#########################################################
australia = function(site){
  discharge = try(get_daily(parameter_type = 'Water Course Discharge',
                            station_number=site,
                            start_date = '1900-01-01',
                            end_date = '2022-12-31'))
  discharge$Q = discharge$Value
  discharge$Date = as.Date(discharge$Timestamp)
  return(discharge)
  if(is.error(discharge)){next}else{
    return(NA)
  }
}


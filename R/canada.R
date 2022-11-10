#' @title canada
#' @name canada
#'
#' @description Provides access to Canadian gauge data
#'
#' @param site Canada gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import dplyr
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @import tidyhydat
#' @examples
#' df = canada("01AD003")
#' plot(df$Date, df$Q, type='l')
#' @export

################################################################################
##Discharge download functions.
################################################################################
qDownload = function(site){
  can = try(hy_daily_flows(site))
  if(is.error(can)){
    return(NA)
  }else{
    can$Q = can$Value
    return(can)
  }
}

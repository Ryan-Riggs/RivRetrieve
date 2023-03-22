#' @title usa
#' @name usa
#'
#' @description Provides access to USA gauge data
#'
#' @param site US gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import dataRetrieval
#' @import data.table
#' @import datasets
#' @import dplyr
#' @import BBmisc
#' @examples
#' df = usa("02471078")
#' plot(df$Date, df$Q, type='l')
#' @export
##Author:Ryan Riggs
##Date: 3/15/2022


# ##Remove bug in dataRetrieval package.
# remove_has_internet <- function()
# {
#   unlockBinding(sym = "has_internet", asNamespace("curl"))
#   assign("has_internet", function() return(TRUE), envir = asNamespace("curl"))
#   lockBinding(sym = "has_internet", asNamespace("curl"))
# }
# remove_has_internet()


################################################################################
##Discharge download and processing functions.
################################################################################
usgs_q_processing = function(usgs_q){
  q_v = as.vector(usgs_q[,4])
  q_c = as.character(usgs_q[4])
  q_n = as.numeric(q_v)
  Q= q_n *0.02832
  usgs_q$Q = Q
  return(usgs_q)
}

usa = function(site){
  usgs = try(usgs_q_processing(rawDailyData <- readNWISdv(site,'00060')))
  if(!is.error(usgs)){
    return(usgs[,c('Date','Q')])
  }
  return(NA)
}


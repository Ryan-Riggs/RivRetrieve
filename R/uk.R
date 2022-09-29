#' @title uk
#' @name uk
#'
#' @description Provides access to UK gauge data
#'
#' @param site UK gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import dplyr
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @examples
#' df = uk("http://environment.data.gov.uk/hydrology/id/stations/3c5cba29-2321-4289-a1fd-c355e135f4cb")
#' plot(df$Date, df$Q, type='l')
#' @export



##Author: Ryan Riggs
##Date: 9/29/2022

##Note: There are quality codes in this database.
##They are part of the output and nothing is currently filtered out.

library(data.table)
##Path\\to\\uk\\sites.csv
sites= fread("E:\\research\\GlobalGaugeData\\UK\\sites\\sites.csv")
################################################################################
##Discharge download functions.
################################################################################

uk = function(site){
  web = site
  first = strsplit(web, "/")
  id = first[[1]][[7]]
  first[[1]][[1]] = paste0(first[[1]][[1]], "/")
  outPath = tempfile()
  download.file(paste0(newWeb,id, dischargeLoc), outPath)
  df = fread(outPath)
  df$Date = df$date
  df$Q = df$value
  return(df)
}

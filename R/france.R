#' @title france
#' @name france
#'
#' @description Provides access to French gauge data
#'
#' @param site French gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import jsonlite
#' @import data.table
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @examples
#' df = france('K027401001')
#' plot(df$Date, df$Q, type='l')
#' @export



##Author: Ryan Riggs
##Date: 3/22/2023
#########################################################
library(rvest)
library(RSelenium)
library(jsonlite)
library(data.table)
library(BBmisc)

##########################################################################################################
##website
##########################################################################################################
station_specific = 'https://hubeau.eaufrance.fr/api/v1/hydrometrie/obs_elab?code_entite='
##########################################################################################################
##Download Q fun: Conversion of divide by 1000 is in the function.
##########################################################################################################
france =function(site){
  web = paste0(station_specific,site,'&date_debut_obs_elab=1800-01-01&date_fin_obs_elab=2022-12-31&grandeur_hydro_elab=QmJ&size=20000')
  df =fromJSON(web)$data
  if(is.null(nrow(df))){return(NULL)}else{
    df=data.table(Q=as.numeric(df$resultat_obs_elab)/1000,
                  Date=as.Date(df$date_obs_elab))
    df = df[df$Q>=0,]
  }
  return(df)
}

## code to prepare `GaugeData` dataset goes here

usethis::use_data(GaugeData, overwrite = TRUE)
file.create("R/data.R")
usethis::use_git()

chile_sites=foreign::read.dbf('E:\\research\\GlobalGaugeData\\Chile\\chile_sites.dbf')
chile_sites = chile_sites[,c('codigo_esta', 'latitud','longitud')]
colnames(chile_sites) = c('site', 'latitude', 'longitude')
chile_sites$site=as.character(chile_sites$site)
chile_sites$latitude=as.numeric(as.character(chile_sites$latitude))
chile_sites$longitude=as.numeric(as.character(chile_sites$longitude))

brazil_sites=data.table::fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\Brazil\\brazil.csv")
brazil_sites=brazil_sites[,c('CODIGO', 'LATITUDE','LONGITUDE')]
colnames(brazil_sites) = c('site', 'latitude', 'longitude')
brazil_sites$site=as.character(brazil_sites$site)
brazil_sites$latitude=as.numeric(brazil_sites$latitude)
brazil_sites$longitude=as.numeric(brazil_sites$longitude)

canada_sites = tidyhydat::allstations
canada_sites=canada_sites[,c('STATION_NUMBER', 'LATITUDE','LONGITUDE')]
colnames(canada_sites) = c('site', 'latitude', 'longitude')
canada_sites$site=as.character(canada_sites$site)
canada_sites$latitude=as.numeric(canada_sites$latitude)
canada_sites$longitude=as.numeric(canada_sites$longitude)

japan_sites = data.table::fread("E:\\research\\GSIM\\GSIM_metadata\\GSIM_catalog\\GSIM_metadata.csv")
japan_sites = japan_sites[japan_sites$reference.db=="mlit",]
japan_sites=japan_sites[,c('reference.no', 'latitude','longitude')]
colnames(japan_sites) = c('site', 'latitude', 'longitude')
japan_sites$site=as.character(japan_sites$site)
japan_sites$latitude=as.numeric(japan_sites$latitude)
japan_sites$longitude=as.numeric(japan_sites$longitude)

uk_sites= data.table::fread("E:\\research\\GlobalGaugeData\\UK\\sites\\sites.csv")
uk_sites=uk_sites[,c('@id', 'lat','long')]
colnames(uk_sites) = c('site', 'latitude', 'longitude')
uk_sites$site=as.character(uk_sites$site)
uk_sites$latitude=as.numeric(uk_sites$latitude)
uk_sites$longitude=as.numeric(uk_sites$longitude)

library(dataRetrieval)
library(dplyr)
code='00060'
states = state.abb
sites_list = list()
for(i in 1:length(states)){
  sites <- whatNWISsites(stateCd = states[i],parameterCd=code,hasDataTypeCd="dv")
  sites_list[[i]] = sites
}
sites = data.table::rbindlist(sites_list)
Gage_df = sites
Gage_df = Gage_df%>%select("site_no", "dec_lat_va", "dec_long_va")
usa_sites=Gage_df
colnames(usa_sites) = c('site', 'latitude', 'longitude')
usa_sites$site=as.character(usa_sites$site)
usa_sites$latitude=as.numeric(usa_sites$latitude)
usa_sites$longitude=as.numeric(usa_sites$longitude)

library(rvest)
library(RSelenium)
library(jsonlite)
library(data.table)
library(BBmisc)
sitesWebsite= 'https://hubeau.eaufrance.fr/api/v1/hydrometrie/referentiel/stations?format=json&size=10000'
french_sites = fromJSON(sitesWebsite)$data
french_sites = french_sites[!is.na(french_sites$date_ouverture_station),]
french_sites=french_sites[,c('code_station', 'latitude_station', 'longitude_station')]
colnames(french_sites) = c('site', 'latitude', 'longitude')
french_sites$site=as.character(french_sites$site)
french_sites$latitude=as.numeric(french_sites$latitude)
french_sites$longitude=as.numeric(french_sites$longitude)




usethis::use_data(chile_sites, brazil_sites,canada_sites,japan_sites,uk_sites,usa_sites,french_sites, overwrite = TRUE)

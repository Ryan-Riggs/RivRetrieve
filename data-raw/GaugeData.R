## code to prepare `GaugeData` dataset goes here

usethis::use_data(GaugeData, overwrite = TRUE)
file.create("R/data.R")
usethis::use_git()

chile_sites=foreign::read.dbf('E:\\research\\GlobalGaugeData\\Chile\\chile_sites.dbf')
chile_sites = chile_sites[,c('codigo_esta', 'latitud','longitud')]
colnames(chile_sites) = c('site', 'latitude', 'longitude')
chile_sites$site=as.character(chile_sites$site)
chile_sites$latitude=as.numeric(chile_sites$latitude)
chile_sites$longitude=as.numeric(chile_sites$longitude)

brazil_sites=data.table::fread("E:\\research\\RatingCurveAnalysis\\GaugeLocations\\Brazil\\brazil.csv")
brazil_sites=brazil_sites[,c('CODIGO', 'LATITUDE','LONGITUDE')]
colnames(brazil_sites) = c('site', 'latitude', 'longitude')
brazil_sites$site=as.character(brazil_sites$site)
brazil_sites$latitude=as.numeric(brazil_sites$latitude)
brazil_sites$longitude=as.numeric(brazil_sites$longitude)

usethis::use_data(chile_sites, brazil_sites, overwrite = TRUE)

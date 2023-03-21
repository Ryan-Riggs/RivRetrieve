## code to prepare `GaugeData` dataset goes here

usethis::use_data(GaugeData, overwrite = TRUE)
file.create("R/data.R")
usethis::use_git()

chile_sites=foreign::read.dbf('E:\\research\\GlobalGaugeData\\Chile\\chile_sites.dbf')
usethis::use_data(chile_sites)

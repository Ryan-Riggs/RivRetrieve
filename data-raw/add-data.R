## TODO

load("data-raw/australia_sites.rda")
load("data-raw/brazil_sites.rda")
load("data-raw/canada_sites.rda")
load("data-raw/chile_sites.rda")
load("data-raw/french_sites.rda")
load("data-raw/japan_sites.rda")
load("data-raw/southAfrican_sites.rda")
load("data-raw/uk_sites.rda")
load("data-raw/usa_sites.rda")

usethis::use_data(
         australia_sites,
         brazil_sites,
         canada_sites,
         chile_sites,
         french_sites,
         japan_sites,
         southAfrican_sites,
         uk_sites,
         usa_sites,
         internal = TRUE,
         overwrite = TRUE
         )

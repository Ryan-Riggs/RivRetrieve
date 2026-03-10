## TODO

load("data-raw/australia_sites.rda")
load("data-raw/brazil_sites.rda")
load("data-raw/canada_sites.rda")
load("data-raw/chile_sites.rda")
load("data-raw/french_sites.rda")
load("data-raw/ireland_sites.rda")
load("data-raw/japan_sites.rda")
load("data-raw/quebec_sites.rda")
load("data-raw/scotland_sites.rda")
load("data-raw/southAfrican_sites.rda")
load("data-raw/sweden_sites.rda")
load("data-raw/uk_sites.rda")
load("data-raw/usa_sites.rda")

usethis::use_data(
         australia_sites,
         brazil_sites,
         canada_sites,
         chile_sites,
         french_sites,
         ireland_sites,
         japan_sites,
         quebec_sites,
         scotland_sites,
         southAfrican_sites,
         sweden_sites,
         uk_sites,
         usa_sites,
         internal = TRUE,
         overwrite = TRUE
         )

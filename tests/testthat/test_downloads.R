context("Download functions")
library(testthat)
library(RivRetrieve)

test_that("Downloading site locations is working", {

  Australia_sites <- australia(sites = TRUE)
  expect_s3_class(Australia_sites,'data.frame')

  Brazil_sites <- brazil(sites = TRUE)
  expect_s3_class(Brazil_sites, "data.frame")

  Chile_sites <- chile(sites = TRUE)
  expect_s3_class(Chile_sites, "data.frame")

  France_sites <- france(sites = TRUE)
  expect_s3_class(France_sites, "data.frame")

  Japan_sites <- japan(sites = TRUE)
  expect_s3_class(Japan_sites, "data.frame")

  SouthAfrica_sites <- southAfrica(sites = TRUE)
  expect_s3_class(SouthAfrica_sites, "data.frame")

  UK_sites <- uk(sites = TRUE)
  expect_s3_class(UK_sites, "data.frame")

  USA_sites <- usa(sites = TRUE)
  expect_s3_class(USA_sites, "data.frame")

})

test_that("Download stage functions are working", {

  Australia_sites <- australia(sites = TRUE)
  australia_df <- australia(Australia_sites$site[1], "stage")
  expect_s3_class(australia_df, "data.frame")

  Brazil_sites <- brazil(sites = TRUE)
  brazil_df <- brazil(Brazil_sites$site[1], "stage")
  expect_s3_class(brazil_df, "data.frame")

  France_sites <- france(sites = TRUE)
  france_df <- france('K027401001', "stage")
  expect_s3_class(france_df, "data.frame")

  SouthAfrica_sites <- southAfrica(sites = TRUE)
  # southAfrica_df <- southAfrica("X3H023", "stage",start_date=as.Date('2005-01-01'),end_date=as.Date('2005-12-31'))
  # expect_s3_class(southAfrica_df, "data.frame")

  UK_sites <- uk(sites = TRUE)
  uk_df <- uk(UK_sites$site[1], "stage",start_date=as.Date('2005-01-01'),end_date=as.Date('2005-12-31'))
  expect_s3_class(uk_df, "data.frame")

  USA_sites <- usa(sites = TRUE)
  usa_df <- usa(USA_sites$site[10], "stage")
  expect_s3_class(usa_df, "data.frame")
})

test_that("Download discharge functions are working", {

  Australia_sites <- australia(sites = TRUE)
  australia_df <- australia(Australia_sites$site[1], "discharge")
  expect_s3_class(australia_df, "data.frame")

  Brazil_sites <- brazil(sites = TRUE)
  brazil_df <- brazil(Brazil_sites$site[1], "discharge")
  expect_s3_class(brazil_df, "data.frame")

  Chile_sites <- chile(sites = TRUE)
  chile_df <- chile(Chile_sites$site[1], "discharge")
  expect_s3_class(chile_df, "data.frame")

  France_sites <- france(sites = TRUE)
  france_df <- france('K027401001', "discharge")
  expect_s3_class(france_df, "data.frame")

  Japan_sites <- japan(sites = TRUE)
  # japan_df <- japan(Japan_sites$site[1], "discharge",start_date=as.Date('2000-01-01'), end_date=as.Date('2001-12-31'))
  # expect_s3_class(japan_df, "data.frame")

  SouthAfrica_sites <- southAfrica(sites = TRUE)
  # southAfrica_df <- southAfrica("X3H023", "discharge",start_date=as.Date('2005-01-01'),end_date=as.Date('2005-12-31'))
  # expect_s3_class(southAfrica_df, "data.frame")

  UK_sites <- uk(sites = TRUE)
  uk_df <- uk(UK_sites$site[1], "discharge")
  expect_s3_class(uk_df, "data.frame")

  USA_sites <- usa(sites = TRUE)
  usa_df <- usa(USA_sites$site[1], "discharge")
  expect_s3_class(usa_df, "data.frame")
})

test_that("Download raw data are working", {

  Australia_sites <- australia(sites = TRUE)
  australia_df <- australia(Australia_sites$site[1], "discharge")
  australia_raw=original(australia_df)
  expect_s3_class(australia_raw, "data.frame")

  Brazil_sites <- brazil(sites = TRUE)
  brazil_df <- brazil(Brazil_sites$site[1], "discharge")
  brazil_raw=original(brazil_df)
  expect_s3_class(brazil_raw, "data.frame")

  Chile_sites <- chile(sites = TRUE)
  chile_df <- chile(Chile_sites$site[1], "discharge")
  chile_raw=original(chile_df)
  expect_s3_class(chile_raw, "data.frame")

  France_sites <- france(sites = TRUE)
  france_df <- france('K027401001', "discharge")
  france_raw=original(france_df)
  expect_s3_class(france_raw, "data.frame")

  Japan_sites <- japan(sites = TRUE)
  # japan_df <- japan(Japan_sites$site[1], "discharge",start_date=as.Date('2000-01-01'), end_date=as.Date('2001-12-31'))
  # japan_raw=original(japan_df)
  # expect_s3_class(japan_raw, "data.frame")

  SouthAfrica_sites <- southAfrica(sites = TRUE)
  # southAfrica_df <- southAfrica("X3H023", "discharge",start_date=as.Date('2005-01-01'),end_date=as.Date('2005-12-31'))
  # southAfrica_raw=original(southAfrica_df)
  # expect_s3_class(southAfrica_raw, "data.frame")

  UK_sites <- uk(sites = TRUE)
  uk_df <- uk(UK_sites$site[1], "discharge")
  uk_raw=original(uk_df)
  expect_s3_class(uk_raw, "data.frame")

  USA_sites <- usa(sites = TRUE)
  usa_df <- usa(USA_sites$site[1], "discharge")
  usa_raw=original(usa_df)
  expect_s3_class(usa_raw, "data.frame")
})

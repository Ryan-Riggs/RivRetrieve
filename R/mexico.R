#' @title mexico
#' @name mexico
#'
#' @description Retrieve Mexican gauge data
#'
#' @param site Mexico gauge number
#' @param variable Character. Either `stage` or `discharge`.
#' @param start_date Character. Optional start date with format
#'   YYYY-MM-DD. Default is 1900-01-01.
#' @param end_date Character. End date with format YYYY-MM-DD.
#'   Default is the current date.
#' @param sites Logical. If TRUE, returns a list of measurement
#'   sites.
#' @param ... Additional arguments. None implemented.
#'
#' @return data frame of discharge time-series
#' @examples
#' \dontrun{
#' start_date <- as.Date("2019-01-01")
#' end_date <- as.Date("2022-12-31")
#' df <- mexico("301011281104010", "discharge", start_date, end_date)
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
mexico <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {

  ## if (sites) {
  ##   return(mexico_sites)
  ## }
  ## start_date <- .get_start_date(start_date)
  ## end_date <- .get_end_date(end_date)
  ## column_name <- .get_column_name(variable)
  ## original_data <- download_japan_data(site, variable, start_date, end_date)
  ## data <- parse_japan_data(original_data)
  ## ## Make sure timeseries is complete
  ## ts <- tibble(Date = seq.Date(start_date, end_date, by = "1 day"))
  ## data <- ts %>% left_join(data, by = "Date")
  ## data <- data %>%
  ##   rename(!!column_name := "Value")
  ## out <- new_tibble(
  ##   data,
  ##   original = original_data,
  ##   class = "rr_tbl"
  ## )
  out <- NULL
  return(out)
}

download_mexico_data <- function(...) {
  url <- "ftp://ftp.conagua.gob.mx/Bandas"
  stations_url <- paste0(url, "/estaciones.xls")
  system(paste0("wget -O /tmp/test.xls ", stations_url))
  system(paste0("curl -s ", url, "/Bases_Datos_Bandas/"))
  data_url <- paste0(url, "/Bases_Datos_Bandas/", "37012.mdb")
  system(paste0("wget -O /tmp/test.mdb ", data_url))
  ## The above code works - currently stuck on reading mdb file (requires RODBC)
}

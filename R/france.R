#' @title france
#' @name france
#'
#' @description Retrieve French gauge data
#'
#' @param site French gauge number
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
#' \donttest{
#' df <- france('K027401001')
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
france <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {

  if (sites) {
    return(french_sites)
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- download_france_data(site, start_date, end_date)
  if(nrow(original_data)==0){return(print('This gauge does not have a record associated with it and/or the agency website is down.'))}
  data <- original_data %>%
    mutate(
      Date = as.Date(!!sym("date_obs_elab")),
      !!column_name := as.numeric(!!sym("resultat_obs_elab")) / 1000.
    ) %>%
    dplyr::select(all_of(c("Date", column_name))) %>%
    arrange(Date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_france_data <- function(site, start_date, end_date) {
  ## FIXME this will only download 20000 records at once, so we need to provide a method to split the download if necessary
  web <- paste0(
    "https://hubeau.eaufrance.fr/api/v1/hydrometrie/obs_elab?code_entite=",
    site,
    "&date_debut_obs_elab=", start_date,
    "&date_fin_obs_elab=", end_date,
    "&grandeur_hydro_elab=QmJ",
    "&size=20000"
  )
  original_data <- fromJSON(web)$data
  original_data <- as_tibble(original_data)
  return(original_data)
}

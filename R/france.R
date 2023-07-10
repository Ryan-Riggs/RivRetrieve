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
#' @param ... Additional arguments. None implemented.
#'
#' @return data frame of discharge time-series
#' @examples
#' \dontrun{
#' df <- france('K027401001')
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
france <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   ...) {

  if (variable == "stage") {
    stop("Stage data is not currently available for France")
  }

  if (is.null(start_date))
    start_date <- "1900-01-01"

  station_specific <- 'https://hubeau.eaufrance.fr/api/v1/hydrometrie/obs_elab?code_entite='
  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  ## Download data
  web <- paste0(
    station_specific, site,
    '&date_debut_obs_elab=1800-01-01&date_fin_obs_elab=2022-12-31&grandeur_hydro_elab=QmJ&size=20000'
  )
  df <- fromJSON(web)$data
  if (is.null(nrow(df))) {
    return(NULL)
  } else {
    df <- tibble(
      Date = as.Date(df$date_obs_elab),
      Q = as.numeric(df$resultat_obs_elab) / 1000
    ) %>%
      arrange(Date) %>%
      filter(Date >= start_date & Date <= end_date)
    ## FIXME - this seems dangerous?
    df <- df[df$Q >= 0, ]
  }
  return(df)
}

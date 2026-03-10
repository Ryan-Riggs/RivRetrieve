#' @title ireland
#' @name ireland
#'
#' @description Retrieve Ireland gauge data
#'
#' @param site Ireland gauge number
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
#' df <- ireland('25014',"discharge")
#' plot(df$Date, df$Q)
#' }
#' @export

ireland <- function(site,
                    variable,
                    start_date = NULL,
                    end_date = NULL,
                    sites = FALSE,
                    ...) {
  if (sites) {
    return(ireland_sites)
  }

  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)

  original_data <- try(download_ireland_data(
    site, variable, start_date, end_date
  ), silent = TRUE)

  if(is.error(original_data) == TRUE | length(original_data) == 0) {
    stop('This gauge does not have a record associated with it and/or the agency website is down.')
  }

  data <- original_data
  column_name = ifelse(variable == "discharge", "Q", "H")

  data <- data %>% dplyr::select(all_of(c("Timestamp", "Value"))) %>%
    rename(Date = "Timestamp", !!column_name := "Value")

  data$Date = as.Date(data$Date)
  data[, 2] = as.numeric(data[, 2])

  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )

  return(out)
}

download_ireland_data = function(site, variable, start_date, end_date) {
  if (variable == "stage") {
    path = paste0('https://waterlevel.ie/hydro-data/data/internet/stations/0/', site, '/S/year.json')
  } else if (variable == "discharge") {
    path = paste0('https://waterlevel.ie/hydro-data/data/internet/stations/0/', site, '/Q/year.json')
  }

  df = fromJSON(path)
  data = as.data.frame(do.call('rbind', df$data))
  colnames(data) = unlist(str_split(df$columns[1], ','))
  data = data[as.Date(data$Timestamp) <= end_date & as.Date(data$Timestamp) >= start_date, ]
  return(data)
}

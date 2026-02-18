#' @title quebec
#' @name quebec
#'
#' @description Retrieve Quebec gauge data
#'
#' @param site quebec gauge number
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
#' site <- "074903"
#' x <- quebec(site, variable = "stage")
#' plot(x$Date, x$H, type='l')
#' }
#' @import data.table fread
#' @export
quebec <- function(site,
                   variable,
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {
  if (sites) {
    return(quebec_sites)
  }
  f <- as.character(site)
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  p_code=ifelse(variable=='discharge','Q','N')
  target_url <- paste0('https://www.cehq.gouv.qc.ca/depot/historique_donnees/fichier/', f,'_',p_code,'.txt')

  out=tempfile()

  download_status <- tryCatch({
    utils::download.file(url = target_url, destfile = out, silent = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!download_status) return(stop('This gauge does not have a record associated with it and/or the agency website is down.'))

  raw_data <- fread(out, fill = TRUE, colClasses = "character",encoding='Latin-1')
  # Find the header row dynamically
  header_idx <- grep('Date', raw_data$V2)

  if (length(header_idx) == 0) {
    return(stop('This gauge does not have a record associated with it and/or the agency website is down.'))
  }

  # Slicing from the row after the header and selecting columns 2 and 3
  original_data <- raw_data[(header_idx + 1):nrow(raw_data),1:4]
  original_data = as_tibble(original_data)
  colnames(original_data) = unlist(raw_data[header_idx,1:4])

  data = as.data.frame(original_data[,2:3])
  data[,1] = as.Date(data[,1],format='%Y/%m/%d')
  data[,2] = as.numeric(data[,2])
  colnames(data)=c('Date',ifelse(p_code=='Q','Q','H'))
  data = data[data$Date>=start_date&data$Date<=end_date,]

  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}


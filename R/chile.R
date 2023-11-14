#' @title chile
#' @name chile
#'
#' @description Retrieve Chilean gauge data
#'
#' @param site Chilean gauge number
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
#' df <- chile('01201005')
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
chile <- function(site,
                  variable = "discharge",
                  start_date = NULL,
                  end_date = NULL,
                  sites = FALSE,
                  ...) {

  if (sites) {
    return(chile_sites)
  }
  if (variable == "stage") {
    stop("Stage data is not currently available for Chile")
  }
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- try(download_chile_data(site),silent=TRUE)
  if(is.error(original_data)){stop('This gauge does not have a record associated with it and/or the agency website is down.')}
  data <- original_data %>%
    unite("Date", "agno", "mes", "dia", sep = "-") %>%
    mutate(
      Date = as.Date(!!sym("Date")),
      valor = as.numeric(!!sym("valor"))
    ) %>%
    rename(!!column_name := "valor") %>%
    arrange(.data$Date) %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_chile_data <- function(site) {
  original <- "https://explorador.cr2.cl/request.php?options={%22variable%22:{%22id%22:%22qflxDaily%22,%22var%22:%22caudal%22,%22intv%22:%22daily%22,%22season%22:%22year%22,%22stat%22:%22mean%22,%22minFrac%22:80},%22time%22:{%22start%22:-946771200,%22end%22:1631664000,%22months%22:%22A%C3%B1o%20completo%22},%22anomaly%22:{%22enabled%22:false,%22type%22:%22dif%22,%22rank%22:%22no%22,%22start_year%22:1980,%22end_year%22:2010,%22minFrac%22:70},%22map%22:{%22stat%22:%22mean%22,%22minFrac%22:10,%22borderColor%22:%227F7F7F%22,%22colorRamp%22:%22Jet%22,%22showNaN%22:false,%22limits%22:{%22range%22:[5,95],%22size%22:[4,12],%22type%22:%22prc%22}},%22series%22:{%22sites%22:[%22"
  ending <- "%22],%22start%22:null,%22end%22:null},%22export%22:{%22map%22:%22Shapefile%22,%22series%22:%22CSV%22,%22view%22:{%22frame%22:%22Vista%20Actual%22,%22map%22:%22roadmap%22,%22clat%22:-18.0036,%22clon%22:-69.6331,%22zoom%22:5,%22width%22:461,%22height%22:2207}},%22action%22:[%22export_series%22]}"
  ## Wait a short time before downloading, to prevent overloading the server
  Sys.sleep(.25)
  outpath <- tempfile()
  website <- paste0(original, site, ending)
  file <- session(website) %>%
    html_element("body") %>%
    html_text("url")
  page <- gsub("(.*)(https://.*)(\"}}})", "\\2", file) %>%
    noquote()
  download.file(page, outpath)
  original_data <- read_delim(outpath, show_col_types = FALSE)
  names(original_data) <- sub("\\s+", "", names(original_data))
  original_data <- as_tibble(original_data)
  return(original_data)
}

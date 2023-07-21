#' @title brazil
#' @name brazil
#'
#' @description Retrieve Brazilian gauge data
#'
#' @param site Brazilian gauge number
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
#' df <- brazil('12650000')
#' plot(df$Date, df$Q, type='l')
#' }
#' @export
brazil <- function(site,
                   variable = "discharge",
                   start_date = NULL,
                   end_date = NULL,
                   sites = FALSE,
                   ...) {

  if (sites) {
    return(brazil_sites)
  }

  if (is.null(start_date))
    start_date <- "1900-01-01"

  ## If `end_date` is not specified then use the current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")

  ## Download data to a temporary location
  link <- "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=3&documentos="
  tmpdir <- tempdir()
  out_path <- tempfile()
  files <- paste0(link, site)
  out <- paste0(out_path, site, ".zip")
  res <- download.file(files, out, method = "curl")
  if (res != 0) {
    stop()
  }
  a <- unzip(out, exdir = tmpdir)
  if (variable == "discharge") {
    f <- unzip(a[grep("^(.*)/vazoes_(.*).zip", a)], exdir = tmpdir)
  } else if (variable == "stage") {
    f <- unzip(a[grep("^(.*)/cotas_(.*).zip", a)], exdir = tmpdir)
  }
  data <- read_hidroweb_data(f)
  data <- parse_hidroweb_data(
    data, variable = variable
  )
  ## TODO ensure output corresponds with Ryan's original function
  ## TODO not sure we should remove this info?
  data <- data %>%
    dplyr::select(all_of(c("date", "Value"))) %>%
    rename(Date = "date") %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  if (variable == "discharge") {
    data <- data %>% rename(Q = "Value")
  } else if (variable == "stage") {
    data <- data %>% rename(H = "Value")
  }
  data
}

read_hidroweb_data <- function(filename, ...) {
  ## TODO unsure how reliable 'skip=13' will be
  data <- suppressWarnings(
    read_delim(
      filename, delim = ";", skip = 13,
      progress = FALSE, show_col_types = FALSE)
  )
  data
}

parse_hidroweb_data <- function(data, variable = "stage", ...) {
  id_cols <- c("EstacaoCodigo", "NivelConsistencia", "Data", "Hora")
  if (variable == "stage") {
    prefix <- "Cota"
  } else if (variable == "discharge") {
    prefix <- "Vazao"
    id_cols <- c(id_cols, "MetodoObtencaoVazoes")
  }
  ## Pivot longer, while keeping values and status flags
  data <- data %>%
    dplyr::select(all_of(id_cols), starts_with(prefix)) %>%
    mutate(across(starts_with(prefix), as.character)) %>%
    rename_with(
      .cols = ends_with("Status"),
      .fn = str_replace, pattern = "Status",
      replace = "_Status"
    ) %>%
    rename_with(
      .cols = matches(paste0(prefix, "[0-9]+$")),
      .fn = str_c,
      "_Value"
    )
  data <- data %>%
    pivot_longer(
      -all_of(id_cols),
      names_to = c("day", ".value"),
      names_sep = "_"
    )

  ## Convert strings to numeric
  data <- data %>%
    mutate(
      Value = gsub(",", "", .data$Value),
      Value = as.numeric(.data$Value),
      Status = gsub(";", "", .data$Status),
      Status = na_if(.data$Status, ""),
      Status = as.numeric(.data$Status)
    )

  ## Get time series
  data <- data %>%
    mutate(Data = as.Date(.data$Data, format = "%d/%m/%Y")) %>%
    mutate(
      year = lubridate::year(.data$Data),
      month = lubridate::month(.data$Data),
      day = gsub(prefix, "", .data$day),
      day = as.numeric(.data$day)
    )

  ## Clean `Hora` column
  data <- data %>%
    mutate(
      Hora = gsub("^(.*) ([0-9]{2}):([0-9]{2}):([0-9]{2})$", "\\2", .data$Hora),
      Hora = as.numeric(.data$Hora)
    )

  ## Note that make_date will give NA for illegitimate
  ## dates (e.g. 31 Feb), so we can filter by NA
  data <- data %>%
    mutate(date = lubridate::make_date(.data$year, .data$month, .data$day)) %>%
    filter(!is.na(.data$date)) %>%
    dplyr::select(-all_of(c("Data", "year", "month", "day"))) %>%
    arrange(.data$date)

  ## Lastly we ensure our object is a complete time series
  ## without any gaps
  complete_ts <-
    tibble(date = seq.Date(data$date[1], rev(data$date)[1], by = "1 day"))
  data <- data %>%
    full_join(complete_ts, by = "date")
  data
}


## ##Author: Ryan Riggs
## ##Date: 9/29/2022

## ################################################################################################
## ##Functions.
## ################################################################################################
## library(data.table)
## link = "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=3&documentos="


## is.error <- function(
##   expr,
##   tell=FALSE,
##   force=FALSE
## )
## {
##   expr_name <- deparse(substitute(expr))
##   test <- try(expr, silent=TRUE)
##   iserror <- inherits(test, "try-error")
##   if(tell) if(iserror) message("Note in is.error: ", test)
##   if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
##   iserror
## }

## substrRight <- function(x, n){
##   substr(x, nchar(x)-n+1, nchar(x))
## }


## brazil = function(site){
##   outpath = tempfile()
##   outpath2 = tempfile()
##   files = paste0(link, site)
##   out = paste0(outpath, site, ".zip")
##   try(download.file(files, out, method = "curl"))
##   a = unzip(out)
##   data = try(read.table(unzip(a[grep("vazoes", a)]), sep = ";", header = TRUE,fileEncoding = "Latin1"))
##   if(!is.error(data)){
##     data1 = data[9:nrow(data),]
##     cols = data1[1:78]
##     data1 = data1[79:length(data1)]
##     starts = data1 == as.character(site)
##     starts = which(starts)
##   }else{next}
##   df = as.data.frame(matrix(numeric(), nrow =length(data1)/length(cols), ncol = length(unlist(cols))))
##   colnames(df) = cols
##   for(j in 1:length(starts)){
##     start = starts[j]
##     end = starts[j]+77
##     dt = data1[start:end]
##     dt = gsub(",", ".", dt)
##     df[j,1:length(cols)] = dt
##   }
##   tab2 = df
##   monthCols = grep("Vazao", colnames(tab2))
##   monthCols = monthCols[-grep("Status",colnames(tab2)[monthCols])]
##   tab2 = melt.data.table(as.data.table(tab2), measure.vars = colnames(tab2)[monthCols])
##   tab2$Day = substrRight(as.character(tab2$variable), 2)
##   tab2$Day = as.numeric(tab2$Day)
##   tab2$month = substr(tab2$Data, 4,5)
##   tab2$year = substr(tab2$Data, 7,10)
##   tab2$Date = paste(tab2$year, tab2$month, tab2$Day, sep = "-")
##   tab2$Date = as.Date(tab2$Date, format = "%Y-%m-%d")
##   out = data.frame(Date=tab2$Date, Q = as.numeric(tab2$value))
##   out = out[order(out$Date),]
##   return(out)
##   }

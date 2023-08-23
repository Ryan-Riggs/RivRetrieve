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
  start_date <- .get_start_date(start_date)
  end_date <- .get_end_date(end_date)
  column_name <- .get_column_name(variable)
  original_data <- download_hidroweb_data(site, variable)
  data <- parse_hidroweb_data(
    original_data, variable = variable
  )
  data <- data %>%
    dplyr::select(all_of(c("date", "Value"))) %>%
    rename(Date = "date") %>%
    rename(!!column_name := "Value") %>%
    filter(.data$Date >= start_date & .data$Date <= end_date)
  out <- new_tibble(
    data,
    original = original_data,
    class = "rr_tbl"
  )
  return(out)
}

download_hidroweb_data <- function(site, variable, ...) {
  ## Download data to a temporary location
  base_url <- "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=3&documentos="
  out <- tempfile()
  res <- download.file(
    paste0(base_url, site), out,
    method = "curl", quiet = TRUE
  )
  if (res != 0) {
    stop()
  }
  tmpdir <- tempdir()
  a <- unzip(out, exdir = tmpdir)
  if (variable == "discharge") {
    f <- unzip(a[grep("^(.*)/vazoes_(.*).zip", a)], exdir = tmpdir)
  } else if (variable == "stage") {
    f <- unzip(a[grep("^(.*)/cotas_(.*).zip", a)], exdir = tmpdir)
  }
  original_data <- read_hidroweb_data(f)
  original_data <- as_tibble(original_data)
  return(original_data)
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

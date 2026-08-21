#' @title argentina
#' @name argentina
#'
#' @description Retrieve Argentine gauge.
#'
#' @param site Argentine gauge number.
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
#' df <- argentina("29", variable = "discharge")
#' plot(df$Date, df$Q, type = "l")
#' }
#' @export
argentina <- function(site,
                      variable = "discharge",
                      start_date = NULL,
                      end_date = NULL,
                      sites = FALSE,
                      ...) {

  if (sites) {
    return(argentina_sites)
  }

  var_id <- .ina_var_id(variable)
  column_name <- if (variable == "discharge") "Q" else "H"

  start_date <- if (is.null(start_date)) as.Date("1900-01-01") else as.Date(start_date)
  end_date   <- if (is.null(end_date)) Sys.Date() else as.Date(end_date)

  original_data <- try(
    download_argentina_data(site, var_id, start_date, end_date),
    silent = TRUE
  )
  if (is.error(original_data) == TRUE) {
    stop(
      "This gauge does not have a record associated with it and/or the agency website is down."
    )
  }
  if (nrow(original_data$dt) == 0) {
    stop(
      "This gauge does not have a record associated with it and/or the agency website is down."
    )
  }

  dt <- original_data$dt
  reported_unit <- original_data$unit

  if (variable == "stage" && !is.null(reported_unit)) {
    if (grepl("cent[i|\u00ed]metro|^cm$", reported_unit, ignore.case = TRUE)) {
      dt[[column_name]] <- dt[[column_name]] / 100
    } else if (!grepl("^m$|metro", reported_unit, ignore.case = TRUE)) {
      warning(
        "Stage unit reported by the API was '", reported_unit, "', which ",
        "isn't recognized as meters or centimeters -- values may not be ",
        "in meters. Check attr(result, \"unit\") before trusting this column."
      )
    }
  } else if (variable == "discharge" && !is.null(reported_unit) &&
             !grepl("m.?3.?/?s", reported_unit, ignore.case = TRUE)) {
    warning(
      "Discharge unit reported by the API was '", reported_unit, "', not ",
      "the expected m^3/s -- values may not be in m3/s."
    )
  }

  dt <- dt[dt[["Date"]] >= start_date & dt[["Date"]] <= end_date]
  if ("Measurement_type" %in% names(dt)) {
    data.table::setorderv(dt, c("Date", "Measurement_type"))
  } else {
    data.table::setorderv(dt, "Date")
  }

  out <- new_tibble(
    tibble::as_tibble(dt),
    original = tibble::as_tibble(original_data$raw),
    class = "rr_tbl"
  )
  attr(out, "unit") <- reported_unit
  attr(out, "procedimiento") <- original_data$procedimiento
  return(out)
}

download_argentina_data <- function(site, var_id, start_date, end_date) {
  headers <- httr::add_headers(`Accept` = "application/json")

  series_url <- paste0(
    "https://alerta.ina.gob.ar/a5/obs/puntual/series",
    "?estacion_id=", site,
    "&var_id=", var_id,
    "&format=json"
  )
  series_resp <- httr::RETRY("GET", series_url, headers, times = 3, quiet = TRUE)
  if (httr::status_code(series_resp) == 404) {
    stop(
      "404 on the series lookup endpoint -- the inferred path ",
      "'/a5/obs/puntual/series' is likely wrong. Check ",
      "https://alerta.ina.gob.ar/a5/apiUI directly for the real path."
    )
  }
  httr::stop_for_status(series_resp, task = "look up series for this station")
  parsed_series <- jsonlite::fromJSON(httr::content(series_resp, as = "text", encoding = "UTF-8"))
  series_list <- parsed_series$rows
  if (is.null(series_list) || length(series_list) == 0 || nrow(series_list) == 0) {
    stop("No series found for this station + variable combination.")
  }

  proc_id <- series_list$procedimiento$id
  keep <- proc_id %in% c(1, 2)
  if (!any(keep)) {
    stop(
      "This station only has a simulated ('Simulado') series for this ",
      "variable -- no direct measurement or rating curve series available, ",
      "and simulated data is excluded by design."
    )
  }
  series_list <- series_list[keep, ]

  all_series_ids <- series_list$id
  all_procedimientos <- series_list$procedimiento$nombre
  unit_abrev <- series_list$unidades$abrev[1]

  ref_start <- format(start_date, "%Y-%m-%dT00:00:00.000Z")
  ref_end   <- format(end_date, "%Y-%m-%dT23:59:59.000Z")
  value_col <- if (var_id == 40) "Q" else "H"

  raw_list <- vector("list", length(all_series_ids))
  for (i in seq_along(all_series_ids)) {
    obs_url <- paste0(
      "https://alerta.ina.gob.ar/a5/obs/puntual/observaciones",
      "?tipo=puntual",
      "&series_id=", all_series_ids[i],
      "&timestart=", ref_start,
      "&timeend=", ref_end,
      "&format=json"
    )
    obs_resp <- httr::RETRY("GET", obs_url, headers, times = 3, quiet = TRUE)
    if (httr::status_code(obs_resp) == 400) {
      stop(
        "400 Bad Request from the observations endpoint. Server said: ",
        httr::content(obs_resp, as = "text", encoding = "UTF-8")
      )
    }
    if (httr::status_code(obs_resp) == 404) {
      stop(
        "404 on the observations endpoint -- the inferred path ",
        "'/a5/obs/puntual/observaciones' is likely wrong. Check ",
        "https://alerta.ina.gob.ar/a5/apiUI directly for the real path."
      )
    }
    httr::stop_for_status(obs_resp, task = "fetch observations from INA alerta5DBIO")
    obs_i <- jsonlite::fromJSON(httr::content(obs_resp, as = "text", encoding = "UTF-8"))
    if (length(obs_i) > 0 && nrow(obs_i) > 0) {
      obs_i <- data.table::as.data.table(obs_i)
      obs_i[["procedimiento"]] <- all_procedimientos[i]
      obs_i[["series_id"]] <- all_series_ids[i]
      raw_list[[i]] <- obs_i
    }
  }
  raw <- data.table::rbindlist(raw_list, use.names = TRUE, fill = TRUE)

  if (nrow(raw) == 0) {
    return(list(
      dt = data.table::data.table(),
      raw = raw,
      unit = unit_abrev,
      procedimiento = all_procedimientos
    ))
  }

  dt <- data.table::copy(raw)
  dt[["Date"]] <- as.Date(dt[["timestart"]])
  data.table::setnames(dt, "valor", value_col)

  types_present <- unique(dt[["procedimiento"]])
  if (length(types_present) <= 1) {
    cols <- c("Date", value_col)
    dt <- dt[, cols, with = FALSE]
    data.table::setorderv(dt, "Date")
  } else {
    data.table::setnames(dt, "procedimiento", "Measurement_type")
    cols <- c("Date", value_col, "Measurement_type")
    dt <- dt[, cols, with = FALSE]
    data.table::setorderv(dt, c("Date", "Measurement_type"))
  }

  list(dt = dt, raw = raw, unit = unit_abrev, procedimiento = all_procedimientos)
}

.ina_var_id <- function(variable) {
  if (variable == "discharge") return(40L)  # Caudal medio diario -- confirmed
  if (variable == "stage") return(39L)      # Altura hidrometrica media diaria -- confirmed
  stop("variable must be either 'discharge' or 'stage'")
}

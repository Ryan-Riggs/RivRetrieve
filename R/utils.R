
.get_start_date <- function(start_date) {
  ## Format start date
  if (is.null(start_date)) {
    start_date <- "1900-01-01"
  }
  return(start_date)
}

.get_end_date <- function(end_date) {
  ## Format end date - if not specified then use current date
  if (is.null(end_date))
    end_date <- Sys.time() %>%
      as.Date() %>%
      format("%Y-%m-%d")
  return(end_date)
}

.get_column_name <- function(variable) {
  ## Get appropriate column name for return object
  if (variable == "stage") {
    colnm <- "H"
  } else if (variable == "discharge") {
    colnm <- "Q"
  } else {
    stop(sprintf("Variable %s is not available", variable))
  }
  return(colnm)
}

## .build_rr_tbl <- function(x, variable, original_data, ...) {
##   if (variable == "discharge") {
##     units <- "m3/s"
##   } else if (variable == "stage") {
##     units <- "m"
##   }
##   out <- new_tibble(
##     data,
##     variable = variable,
##     original = original_data,
##     class = c("rr_tbl")
##   )
## }

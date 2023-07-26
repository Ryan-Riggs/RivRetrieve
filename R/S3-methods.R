#' Get original data
#'
#' @param x Tibble.
#' @param ... Additional arguments. None implemented.
#'
#' @return list
#' @export
original <- function(x, ...) {
  UseMethod("original")
}

#' @export
original.rr_tbl <- function(x, ...) {
  return(attr(x, "original"))
}

#' Plot values
#'
#' @param x Tibble.
#' @param ... Additional arguments. None implemented.
#'
#' @return ggplot2
#' @export
plot.rr_tbl <- function(x, ...) {
  ## varname <- metadata(x)$VariableName
  ## units <- metadata(x)$UnitAbbreviation
  ## if (is.na(units) | nchar(units) == 0) {
  ##   units <- ""
  ## }
  ## y_label <- "TODO" #sprintf("%s [%s]", varname, units)
  ## title <- "TODO" #metadata(x)$citation
  ## p <- ggplot(data = x, aes_string(x = "Date", y = "Q")) +
  ##   geom_line() +
  ##   ylab(y_label) +
  ##   xlab("Time") +
  ##   ggtitle(title)
  ## p
  return(plot(x$Date, x$Q, ...))
}

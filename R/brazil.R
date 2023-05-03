#' @title brazil
#' @name brazil
#'
#' @description Provides access to Brazilian gauge data
#'
#' @param site Brazilian gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import dplyr
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @examples
#' df = brazil('12650000')
#' plot(df$Date, df$Q, type='l')
#' @export



##Author: Ryan Riggs
##Date: 9/29/2022

################################################################################################
##Functions.
################################################################################################
library(data.table)
link = "https://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=3&documentos="


is.error <- function(
  expr,
  tell=FALSE,
  force=FALSE
)
{
  expr_name <- deparse(substitute(expr))
  test <- try(expr, silent=TRUE)
  iserror <- inherits(test, "try-error")
  if(tell) if(iserror) message("Note in is.error: ", test)
  if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
  iserror
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


brazil = function(site){
  outpath = tempfile()
  outpath2 = tempfile()
  files = paste0(link, site)
  out = paste0(outpath, site, ".zip")
  try(download.file(files, out, method = "curl"))
  a = unzip(out)
  data = try(read.table(unzip(a[grep("vazoes", a)]), sep = ";", header = TRUE,fileEncoding = "Latin1"))
  if(!is.error(data)){
    data1 = data[9:nrow(data),]
    cols = data1[1:78]
    data1 = data1[79:length(data1)]
    starts = data1 == as.character(site)
    starts = which(starts)
  }else{next}
  df = as.data.frame(matrix(numeric(), nrow =length(data1)/length(cols), ncol = length(unlist(cols))))
  colnames(df) = cols
  for(j in 1:length(starts)){
    start = starts[j]
    end = starts[j]+77
    dt = data1[start:end]
    dt = gsub(",", ".", dt)
    df[j,1:length(cols)] = dt
  }
  tab2 = df
  monthCols = grep("Vazao", colnames(tab2))
  monthCols = monthCols[-grep("Status",colnames(tab2)[monthCols])]
  tab2 = melt.data.table(as.data.table(tab2), measure.vars = colnames(tab2)[monthCols])
  tab2$Day = substrRight(as.character(tab2$variable), 2)
  tab2$Day = as.numeric(tab2$Day)
  tab2$month = substr(tab2$Data, 4,5)
  tab2$year = substr(tab2$Data, 7,10)
  tab2$Date = paste(tab2$year, tab2$month, tab2$Day, sep = "-")
  tab2$Date = as.Date(tab2$Date, format = "%Y-%m-%d")
  out = data.frame(Date=tab2$Date, Q = as.numeric(tab2$value))
  out = out[order(out$Date),]
  return(out)
  }








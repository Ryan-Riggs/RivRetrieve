#' @title southAfrica
#' @name southAfrica
#'
#' @description Provides access to French gauge data
#'
#' @param site South African gauge number
#'
#' @return data frame of discharge time-series
#' @import devtools
#' @import RSelenium
#' @import jsonlite
#' @import data.table
#' @import BBmisc
#' @import rvest
#' @import data.table
#' @examples
#' df = southAfrica('X3H023')
#' plot(df$Date, df$Q, type='l')
#' @export



##Author: Ryan Riggs
##Date: 3/24/2023
#########################################################
##########################################################################################################
##website
##########################################################################################################
southAfrica=function(f){
  web='https://www.dws.gov.za/Hydrology/Verified/HyData.aspx?Station='
  end='100.00&DataType=Daily&StartDT=1900-01-01&EndDT=2023-03-22&SiteType=RIV'
  comb=paste0(web, f, end)
  data=html_session(comb)%>%html_element('body')%>%html_text('pre')
  data = str_split(data, '\n')
  data = unlist(data)
  data = as.list(data)
  data = data[11:length(data)]
  data_sub=lapply(data, function(x){
    sub=x%>%str_split(' +')
    sub=unlist(sub)
    output=data.table(t(sub))
    if(ncol(output)!=3){
      return(NA)
    }
    colnames(output) = c('Date', 'Q', 'Quality')
    output$Q=as.numeric(output$Q)
    output$Date = as.Date(output$Date, format='%Y%m%d')
    return(output)
  })
  data_sub = data_sub[!is.na(data_sub)]
  data_sub = rbindlist(data_sub)
  return(data_sub)
}

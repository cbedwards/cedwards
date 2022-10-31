#' Converting day of year number to month and day
#'
#' Translates numbers into human-readable day and month, useful when working with phenology data.
#' Note that this function treats decimals as their floor, so `doy_2md(doy = 5.8)` returns "January 05".
#' By default, assumes a non-leap-year.
#'
#' @param doy numeric or vector of numerics representing the day of year.
#' @param short Do we want months abbreviated? Defaults to `FALSE`; if `TRUE`, longer month names like January will be represented with their abbreviations. Useful for axes labels.
#' @param leap Do we want to assume a leap year? Defaults to `FALSE`; if `TRUE`, uses a 29-day February.
#' @returns A character string or vector of strings, with human readable day/month.
#' @keywords date
#' @export
#' @examples
#' doy_2md(18)
#' doy_2md(c(15, 29.8, 180))
#' doy_2md(c(15, 29.8, 180), short=TRUE)


doy_2md=function(doy, short = FALSE, leap = FALSE){
  if(leap){
    orig = "2020-01-01"
  }else{
    orig = "2019-01-01"
  }
  ymd=as.Date(doy-1, origin=orig)
  if(short){
    res = format(ymd, "%b %d")
  }else{
    res = format(ymd, "%B %d")
  }
  return(res)
}
## example usage:
# #generate 30 observations from day of year 40 to 150
# doy=sample(40:150,30)
# #generate gaussian counts with noise
# count=exp(-(doy-100)^2/100)+(runif(30)-.5)*.1
# plot(doy,count)
# # now plot with day-month references
# plot(doy,count, xaxt="n")
# #Make sequence of days to label. Here, 5 days from day 40 to 150
# at=round(seq(40,150, length=5))
# axis(1,at=at, labels=doy_2md(at))

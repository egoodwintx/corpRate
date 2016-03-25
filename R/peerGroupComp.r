# peerGroupCompR.r
# part of the package used for comparing tickers in a peer group for performance
#
#

library(quantmod)
library(readxl)
library(dplyr)

#' Data Frame Medians
#'
#' Retrieves the median of rows in data frames
#'
#' @param data frame
#' @return numeric vector of medians for each row of data frame
#' @seealso \code{\link{data.frame}} which this function wraps
#' @export
#' @examples
#' median.data.frame(df)
#' median.data.frame(data.frame(a=c(1,5,12), b=c(4,8,15)))
median.data.frame <- function(x, na.rm = FALSE) {
  sapply(x, function(y, na.rm = FALSE)
    if (is.factor(y))
      NA
    else
      median(y, na.rm = na.rm), na.rm = na.rm)
}

## ticker list for unit testing
tickers = c(
  "CJES",
  "CLB",
  "DRQ",
  "EXTN",
  "FET",
  "HLX",
  "HOS",
  "MTRX",
  "MDR",
  "NR",
  "OII",
  "OIS",
  "RES",
  "CKH"
)


#' Zoo Tickers
#'
#' return a zoo object of time series performance for each
#' ticker in a vector from 2012
#'
#' @param vector
#' @return zoo
#' @seealso
#' @export
#' @examples
#' peerGroupZoo(c("IBM", "AAPL", "MSFT"))
peerGroupZoo = function(tickers) {
  peergroup = list()

  for (i in 1:length(tickers)) {
    peergroup[[i]] = getSymbols(tickers[i],
                                auto.assign = FALSE,
                                return.class = "xts")
  }

  monthlys = list()
  boundmonthlys = xts()
  for (i in 1:length(tickers)) {
    monthlys[[i]] = monthlyReturn(peergroup[[i]]["2012/"], start = "2012-01-01")
    boundmonthlys = cbind(boundmonthlys, monthlys[[i]])
  }
  ## convert to Date format
  index(boundmonthlys) = as.Date(as.character(index(boundmonthlys)), format =
                                   "%Y-%m-%d")
  names(boundmonthlys) = tickers
  boundmonthlys
}

# bmdf = as.data.frame.ts(boundmonthlys)
# median.data.frame(bmdf, na.rm = TRUE)
# q
# ## plot all the monthly returns
# tsRainbow = rainbow(ncol(boundmonthlys))
# ts.plot(
#   boundmonthlys,
#   gpars = list(ylab = "Monthly Returns",
#                main = "Oil Service Peer Group Returns",
#                col = tsRainbow)
# )
# legend(
#   x = "bottomleft",
#   legend = tickers,
#   lty = 1,
#   col = tsRainbow
# )

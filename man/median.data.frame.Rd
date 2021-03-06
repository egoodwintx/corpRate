% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peerGroupComp.r
\name{median.data.frame}
\alias{median.data.frame}
\title{Data Frame Medians}
\usage{
\method{median}{data.frame}(x, na.rm = FALSE)
}
\arguments{
\item{data}{frame}
}
\value{
numeric vector of medians for each row of data frame
}
\description{
Retrieves the median of rows in data frames
}
\examples{
median.data.frame(df)
median.data.frame(data.frame(a=c(1,5,12), b=c(4,8,15)))
}
\seealso{
\code{\link{data.frame}} which this function wraps
}


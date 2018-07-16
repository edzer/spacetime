\name{na.locf}
\alias{na.locf}
\alias{na.approx}
\alias{na.spline}
\alias{na.locf.STFDF}
\alias{na.approx.STFDF}
\alias{na.spline.STFDF}
\title{ replace NA attribute values; disaggregation time series }
\description{ replace NA attribute values in time series, using last or
next observation, or using (temporal) interpolation, and disaggregation }
\usage{ 
\method{na.locf}{STFDF}(object, na.rm = FALSE, ...)
\method{na.approx}{STFDF}(object, x = time(object), xout, ..., na.rm = TRUE)
\method{na.spline}{STFDF}(object, x = time(object), xout, ..., na.rm = TRUE)
}
\arguments{
\item{object}{ object of class \code{STFDF}, with potentially NA values }
\item{na.rm}{ logical; need non-replaced NA values be removed? }
\item{x}{ times at which observations are taken; should not be modified }
\item{xout}{ if present, new times at which the time series should be
approximated (disaggregated) }
\item{...}{ passed on to underlying zoo functions; see details }
}
\value{
object of class \code{STFDF}, with \code{NA} values replaced.
}
\details{
details are found in
\link[zoo]{na.locf},
\link[zoo]{na.approx},
\link[zoo]{na.spline}.
}
\author{Edzer Pebesma}
\references{ http://www.jstatsoft.org/v51/i07/ }
\examples{
# toy example:
library(sp)
pts = SpatialPoints(cbind(c(0,1),c(0,1)))
Sys.setenv(TZ="GMT")
tm = seq(as.POSIXct("2012-11-25"), as.POSIXct("2012-11-30"), "1 day")
df = data.frame(a = c(NA,NA,2,3,NA,NA,NA,2,NA,NA,4,NA), b = c(NA,2,3,4,5,1,2,NA,NA,NA,NA,3))
x = STFDF(pts, tm, df)
as(x, "xts")
as(na.locf(x), "xts")
as(na.locf(x, fromLast = TRUE), "xts")
as(na.locf(na.locf(x), fromLast = TRUE), "xts")
# drops first record:
as(na.approx(x[,,1]), "xts")
# keep it:
cbind(as(na.approx(x[,,1], na.rm=FALSE), "xts"),
as(na.approx(x[,,2]), "xts"))
cbind(as(na.spline(x[,,1]), "xts"),
as(na.spline(x[,,2]), "xts"))
#disaggregate:
xout = seq(start(x), end(x), "6 hours")
as(na.approx(x[,,1], xout = xout), "xts")
as(na.spline(x[,,1], xout = xout), "xts")
as(na.spline(x[,,2], xout = xout), "xts")

# larger/real data:
data(air)
rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
# fill NA's with last non-NA
r = na.locf(rural)
# sample (NOT aggregate) to monthly:
m = seq(start(rural), end(rural), "1 month")
stplot(na.approx(rural[1:20,"2003::2005"], xout = m), mode = 'ts')
}
\keyword{manip}

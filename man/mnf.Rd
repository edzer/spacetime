\name{mnf}
\alias{mnf}
\alias{mnf.RasterBrick}
\alias{mnf.RasterStack}
\alias{mnf.SpatialGridDataFrame}
\alias{mnf.SpatialPixelsDataFrame}
\alias{mnf.matrix}
\alias{mnf.mts}
\alias{mnf.zoo}
\alias{mnf.STSDF}
\alias{mnf.STFDF}
\title{Generic mnf method}
\description{ Compute mnf from spatial, temporal, or spatio-temporal data }
\usage{
mnf(x, ...)
\method{mnf}{matrix}(x, ..., Sigma.Noise, use = "complete.obs")
\method{mnf}{mts}(x, ..., use = "complete.obs")
\method{mnf}{zoo}(x, ..., use = "complete.obs")
\method{mnf}{SpatialPixelsDataFrame}(x, ..., use = "complete.obs")
\method{mnf}{SpatialGridDataFrame}(x, ..., Sigma.Noise, use = "complete.obs")
\method{mnf}{RasterStack}(x, ..., use = "complete.obs")
\method{mnf}{RasterBrick}(x, ..., use = "complete.obs")
\method{mnf}{STSDF}(x, ..., use = "complete.obs", mode = "temporal")
\method{mnf}{STFDF}(x, ..., use = "complete.obs", mode = "temporal")
}
\arguments{
\item{x}{object for which an mnf method is available}
\item{...}{ignored}
\item{Sigma.Noise}{Noise covariance matrix; when missing, estimated from the data by using the
covariance of lag-one spatial or temporal differences (MAF)}
\item{use}{method to deal with missing values when computing covariances; see \link{cov}}
\item{mode}{for \code{ST} objects: if \code{"temporal"}, compute covariances in time dimension,
if \code{"spatial"}, compute them in spatial dimension.}
}
\value{ object of class \code{(c("mnf", "prcomp")}; see \link{prcomp}. Additional elements
are \code{values}, containing the eigenvalues.  }
\details{ Uses MAF (Min/max Autocorrelation Factors) to estimate the
noise covariance.  This implementation estimates the noise covariance
by \eqn{0.5 \mbox{Cov}(Z(s)-Z(s+\Delta))}, so that eigenvalues can be
directly interpreted as approximate estimates of the noice covariance. }
\seealso{http://r-spatial.org/r/2016/03/09/MNF-PCA-EOF.html }
\examples{
# temporal data:
set.seed(13531) # make reproducible
s1 = arima.sim(list(ma = rep(1,20)), 500)
s2 = arima.sim(list(ma = rep(1,20)), 500)
s3 = arima.sim(list(ma = rep(1,20)), 500)
s3 = s3 + rnorm(500, sd = 10)
d = cbind(s1,s2,s3)
plot(d)
m = mnf(d)
m
summary(m)
plot(predict(m))

# spatial example:
\dontrun{
library(sp)
grd = SpatialPoints(expand.grid(x=1:100, y=1:100))
gridded(grd) = TRUE
fullgrid(grd) = TRUE
pts = spsample(grd, 50, "random")
pts$z = rnorm(50)
library(gstat)
v = vgm(1, "Sph", 90)
out = krige(z~1, pts, grd, v, nmax = 20, nsim = 4)
out[[3]] = 0.5 * out[[3]] + 0.5 * rnorm(1e4)
out[[4]] = rnorm(1e4)
spplot(out, as.table = TRUE)
m = mnf(out)
m
summary(m)
}
if (require(gstat)) {
 data(wind)
 library(sp)
 wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
 wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
 coordinates(wind.loc) = ~x+y
 proj4string(wind.loc) = "+proj=longlat +datum=WGS84"

 # match station order to names in wide table:
 stations = 4:15
 wind.loc = wind.loc[match(names(wind[stations]), wind.loc$Code),]
 row.names(wind.loc) = wind.loc$Station
 wind$time = ISOdate(wind$year+1900, wind$month, wind$day, 0)
 space = list(values = names(wind)[stations])
 wind.st = stConstruct(wind[stations], space, wind$time, SpatialObj = wind.loc, interval = TRUE)
 m = mnf(wind.st)
 m
 plot(m)
 stplot(predict(m), mode = "tp")
}

}

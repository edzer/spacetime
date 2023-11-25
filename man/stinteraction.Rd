\name{stInteraction}
\alias{stInteraction}
\title{ subtract marginal (spatial and temporal) means from observations }
\description{subtract marginal (spatial and temporal) means from observations }
\usage{ 
stInteraction(x, ...)
}
\arguments{
\item{x}{ object of class \code{STFDF}}
\item{...}{ arguments passed to \link{rowMeans}, \link{colMeans} and \link{mean}, such as \code{na.rm=TRUE}}
}
\value{
object of class \link{STFDF} with each attribute replaced by its
residual, computed by \eqn{y_{ij}=x_{ij}-m_{.j}m{i.}/m} with \eqn{m} the
grand mean, \eqn{m_{.j}} the temporal mean, \eqn{m_{i.}} the spatial mean
and \eqn{m} the grand mean.
}
\examples{

if (require(gstat)) {
library(sp)
data(wind)
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
wind.st = stConstruct(wind[stations], space, wind$time, SpatialObj = wind.loc)

wind.sti = stInteraction(wind.st)
# temporal means for any station should be zero:
c(mean(wind.sti[3,]),
# spatial mean for each time step should be zero:
mean(wind.sti[,5][[1]]))
}

}
\keyword{manip}

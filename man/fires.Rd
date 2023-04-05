\name{fires}
\alias{fires}

\title{Northern Los Angeles County Fires}

\usage{data(fires)}

\format{
  A data frame with 313 observations with day of occurrence, x and
  y coordinates.
}


\description{
  Wildfire occurrences in Northern Los Angeles County, California
  between 1976 and 2000.  The spatial units are in scaled feet, taken
  from the NAD 83 state-plane coordinate system.  One unit is equivalent
  to 100,000 feet or 18.9 miles.  The times for the points were produced
  by the \code{date} package and represent the number of days since
  January 1, 1960.
}

\author{ Roger Peng, taken from (non-CRAN) package ptproc, 

\url{https://www.biostat.jhsph.edu/~rpeng/software/index.html};

example code by Roger Bivand.
}

\examples{
data(fires)
fires$X <- fires$X*100000
fires$Y <- fires$Y*100000
library(sp)
coordinates(fires) <- c("X", "Y")
proj4string(fires) <- CRS("+init=epsg:2229 +ellps=GRS80")
dates <- as.Date("1960-01-01")+(fires$Time-1)
Fires <- STIDF(as(fires, "SpatialPoints"), dates, data.frame(time=fires$Time))
library(mapdata)
if (require(sf)) {
 m <- map("county", "california", xlim=c(-119.1, -117.5), 
 	ylim=c(33.7, 35.0), plot=FALSE, fill=TRUE)
 m.sf <- st_transform(st_as_sfc(m), "EPSG:2229")
 cc <- as(m.sf, "Spatial")
 plot(cc, xlim=c(6300000, 6670000), ylim=c(1740000, 2120000))
 plot(slot(Fires, "sp"), pch=3, add=TRUE)
 stplot(Fires, sp.layout=list("sp.lines", cc))
}
}

\keyword{datasets}

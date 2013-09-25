\name{stConstruct}
\alias{stConstruct}
\title{ create ST* objects from long or wide tables }
\description{ create ST* objects from long or wide tables }
\usage{ 
stConstruct(x, space, time, SpatialObj = NULL, TimeObj = NULL, 
	crs = CRS(as.character(NA)), interval, endTime)
}
\arguments{
\item{x}{ object of class \code{matrix} or \code{data.frame},
holding the long, space-wide or time-wide table; see details. }
\item{space}{ in case \code{x} is a long table,
character or integer holding the column index in \code{x} where the spatial
coordinates are (if length(space)==2) or where the ID of the spatial
location is (if (length(space)==1). If \code{x} is a space-wide table,
a list with each (named) list element a set of columns that together
form a variable }
\item{time}{ in case \code{x} is a long table, character or integer 
indicating the column in \code{x} with times; }
\item{SpatialObj}{ object of class \link{Spatial-class}, containing the
locations of a time-wide table, or the locations of a long table  }
\item{TimeObj}{ in case of space-wide table, 
object of class \link{xts}, containing the times for each 
of the columns in a list element of \code{space}
}
\item{crs}{ object of class \link{CRS-class}; only used when coordinates
are in \code{x} and no CRS can be taken from \code{SpatialObj}}
\item{interval}{ logical; specifies whether time should reflect time
instance (FALSE) or time intervals (TRUE). If omitted, defaults values
depend on the class }
\item{endTime}{ vector of \code{POSIXct}, specifying (if present) the end
points of observation time intervals }
}
\details{
For examples, see below.

A long table is a data.frame with each row holding a single
observation in space-time, and particular columns in this table
indicate the space (location or location ID) and time.

A space-wide table is a table in which different columns refer to
different locations, and each row reflects a particular observation
time.

A time-wide table is a table where different times of a particular
characteristic are represented as different colunns; rows in the
table represent particular locations or location IDs.
}
\value{
Depending on the arguments, an object of class \code{STIDF}
or \code{STFDF}.
}
\references{ http://www.jstatsoft.org/v51/i07/ }
\examples{
# example 0: construction of STFDF from long table:
library(maps)
states.m = map('state', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(states.m$names, ":"), function(x) x[1])
 
library(maptools)
states = map2SpatialPolygons(states.m, IDs=IDs)

library(plm)
data(Produc)

yrs = 1970:1986
t = as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT")
# deselect District of Columbia, polygon 8, which is not present in Produc:
Produc.st = STFDF(states[-8], t, Produc[(order(Produc[,2], Produc[,1])),])

# example 1: st from long table, with states as Spatial object:
# use Date format for time:
Produc$time = as.Date(paste(yrs, "01", "01", sep = "-"))
# take centroids of states:
xy = coordinates(states[-8])
Produc$x = xy[,1]
Produc$y = xy[,2]
#using stConstruct, use polygon centroids for location:
x = stConstruct(Produc, c("x", "y"), "time", interval = TRUE)
class(x)
stplot(x[,,"unemp"])

# alternatively, pass states as SpatialObj:
Produc$state = gsub("TENNESSE", "TENNESSEE", Produc$state)
Produc$State = gsub("_", " ", tolower(Produc$state))
x = stConstruct(Produc, "State", "time", states[-8])
class(x)
all.equal(x, Produc.st, check.attributes = FALSE)

# stConstruct multivariable, time-wide
library(maptools)
fname = system.file("shapes/sids.shp", package="maptools")[1]
nc = readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
timesList = list(
	BIR=c("BIR74", "BIR79"),  # sets of variables that belong together
	NWBIR=c("NWBIR74", "NWBIR79"), # only separated by space
	SID=c("SID74", "SID79")
)
t = as.Date(c("1974-01-01","1979-01-01"))
nc.st = stConstruct(as(nc, "data.frame"), geometry(nc), timesList,
	TimeObj = t, interval = TRUE)

# stConstruct multivariable, space-wide
if (require(gstat)) {
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"

# match station order to names in wide table:
stations = 4:15
wind.loc = wind.loc[match(names(wind[stations]), wind.loc$Code),]
row.names(wind.loc) = wind.loc$Station
# convert to utm zone 29, to be able to do interpolation in
# proper Euclidian (projected) space:

# create time variable
wind$time = ISOdate(wind$year+1900, wind$month, wind$day, 0)

w = STFDF(wind.loc, wind$time, 
	data.frame(values = as.vector(t(wind[stations]))))
space = list(values = names(wind)[stations])
wind.st = stConstruct(wind[stations], space, wind$time, SpatialObj = wind.loc, interval = TRUE)
all.equal(w, wind.st)
class(wind.st)
}
}
\keyword{manip}

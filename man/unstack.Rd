\name{unstack}
\alias{unstack.STFDF}
\alias{as.data.frame.STFDF}
\title{ write STFDF to table forms }
\description{ create table forms of STFDF objects }
\usage{ 
\method{unstack}{STFDF}(x, form, which = 1, ...)
\method{as.data.frame}{STFDF}(x, row.names, ...)
}
\arguments{
\item{x}{ object of class \code{STFDF}}
\item{form}{ formula; can be omitted }
\item{which}{ column name or number to have unstacked }
\item{row.names}{ row.names for the data.frame returned }
\item{...}{ arguments passed on to the functions \link[utils]{unstack} 
or \link{as.data.frame}}
}
\value{
\code{unstack} returns the data in wide format, with each
row representing a spatial entity and each column a time; see
\link[utils]{unstack} for details and default behaviour.

\code{as.data.frame} returns the data.frame in long format,
where the coordinates of the spatial locations (or line starting
coordinates, or polygon center points) and time stamps are recycled
accordingly.
}
\examples{
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
library(sp)
sp = SpatialPoints(sp)
library(xts)
time = xts(1:4, as.POSIXct("2010-08-05")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
as.data.frame(stfdf, row.names = IDs)
unstack(stfdf)
t(unstack(stfdf))
unstack(stfdf, which = 2)
}
\keyword{manip}

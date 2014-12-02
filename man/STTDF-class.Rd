\name{STTDF-class}
\docType{class}
\alias{STT-class}
\alias{STTDF-class}
\alias{STT}
\alias{STTDF}
\alias{ltraj-class}
\alias{coerce,STTDF,ltraj-method} 
\alias{coerce,ltraj,STTDF-method}
\alias{[,STT-method}
\alias{[,STTDF-method}
\alias{geometry,STTDF-method}
\alias{plot,STT,missing-method}
\title{Class "STTDF"}
\description{ A class for spatio-temporal trajectory data }
\section{Objects from the Class}{
Objects of this class carry sparse (irregular) space/time data }
\section{Slots}{
  \describe{
    \item{\code{sp}:}{Object of class \code{"Spatial"}, containing the bounding
	box of all trajectories}
    \item{\code{time}:}{Object of class \code{"xts"}, containing the temporal 
	bounding box of all trajectories}
	\item{\code{traj}:}{Object of class \code{list}, each element holding
	an \link{STI} object reflecting a single trajectory; }
	\item{\code{data}:}{Object of class \code{data.frame}, which holds
	the data values for each feature in each trajectory }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "STTDF")}: select trajectories, based
	on index, or spatial and/or temporal predicates } 

    % \item{plot}{\code{signature(x = "Spatial", y = "missing")}: plot method
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  \S4method{coerce}{STTDF,ltraj}(from, to, strict=TRUE)
  \S4method{coerce}{ltraj,STTDF}(from, to, strict=TRUE)
}

\arguments{
\item{from}{from object}
\item{to}{target class}
\item{strict}{ignored}
}

\note{ The \code{data.frame} needs to have a column called
\code{burst} which is a factor (or character) and contains the
grouping of observations that come from a continuous sequence of
observations. In addition, a column \code{id} is used to identify
individual items. }

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }
\examples{
library(sp)
m = 3# nr of trajectories
n = 100 # length of each
l = vector("list", m)
t0 = as.POSIXct("2013-05-05",tz="GMT")
set.seed(1331) # fix randomness
for (i in 1:m) {
    x = cumsum(rnorm(n))
    y = cumsum(rnorm(n))
    sp = SpatialPoints(cbind(x,y))
    #t = t0 + (0:(n-1) + (i-1)*n) * 60
    t = t0 + (0:(n-1) + (i-1)*n/2) * 60
    l[[i]] = STI(sp, t)
}
stt= STT(l)
sttdf = STTDF(stt, data.frame(attr = rnorm(n*m), id = paste("ID", rep(1:m, each=n))))
x = as(stt, "STI")
stplot(sttdf, col=1:m, scales=list(draw=TRUE))
stplot(sttdf, by = "id")
stplot(sttdf[1])
stplot(sttdf[1])

# select a trajectory that intersect with a polygon
p = Polygon(cbind(x=c(-20,-15,-15,-20,-20),y=c(10,10,15,15,10)))
pol=SpatialPolygons(list(Polygons(list(p), "ID")))
library(rgeos)
stplot(sttdf[pol])
names(sttdf[pol]@traj)
stplot(sttdf[1:2],col=1:2)
stplot(sttdf[,t0])
stplot(sttdf[,"2013"])
stplot(sttdf[pol,"2013"])
is.null(sttdf[pol,t0])
}
\keyword{classes}

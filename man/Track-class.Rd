\name{Track-class}
\docType{class}
\alias{Track-class}
\alias{Tracks-class}
\alias{TracksCollection-class}
\alias{segments-class}
\alias{Track}
\alias{Tracks}
\alias{TracksCollection}
\alias{TrackSummary}
\alias{TracksSummary}
\alias{[,Track-method} 
\alias{[,Tracks-method} 
\alias{[,TracksCollection-method} 
\alias{coerce,Track,data.frame-method} 
\alias{coerce,Tracks,data.frame-method} 
\alias{coerce,TracksCollection,data.frame-method}
\alias{coordnames,Track-method} 
\alias{coordnames,Tracks-method} 
\alias{coordnames,TracksCollection-method}
\alias{aggregate,Track-method}
\alias{aggregate,Tracks-method}
\alias{aggregate,TracksCollection-method}
\alias{plot,TracksCollection,ANY-method}
\alias{stplot,TracksCollection-method}
\alias{generalize,Track-method}

\title{Classes "Track", "Tracks", and "TracksCollection"}
\description{ Classes for representing sets of trajectory data, 
with attributes, for different IDs (persons, objects, etc) }
\section{Objects from the Class}{ 
Objects of class \code{Track} extend \link{STIDF-class} 
and contain single trips or tracks, 
objects of class \code{Tracks} contain multiple \code{Track} objects
for a single ID (person, object or tracking device), objects of class
\code{TracksCollection} contain multiple \code{Tracks} objects for
different IDs.
}
\section{Slots of class "Track"}{
  \describe{
	\item{\code{sp}:}{spatial locations of the track points, with length n}
	\item{\code{time}:}{time stamps of the track points}
	\item{\code{endTime}:}{end time stamps of the track points}
	\item{\code{data}:}{\code{data.frame} with n rows, containing
	attributes of the track points}
    \item{\code{connections}:}{\code{data.frame}, with n-1 rows, containing
	attributes between the track points such as distance and speed }
  }
}
\section{Slots of class "Tracks"}{
  \describe{
    \item{\code{tracks}:}{\code{list} with \code{Track} objects, of length m}
    \item{\code{tracksData}:}{\code{data.frame} with m rows, containing 
	summary data for each \code{Track} object}
  }
}
\section{Slots of class "TracksCollection"}{
  \describe{
    \item{\code{tracksCollection}:}{\code{list} \code{Tracks} objects, of
		length p}
    \item{\code{tracksCollectionData}:}{\code{data.frame} with p rows,
	containing summary data for each \code{Tracks} object}
  }
}
\section{Methods}{
  \describe{
   \item{coerce}{Track,data.frame}{coerce to \code{data.frame}}
   \item{coerce}{Tracks,data.frame}{coerce to \code{data.frame}}
   \item{coerce}{TracksCollection,data.frame}{coerce to \code{data.frame}}
   \item{plot}{\code{signature(x = "TracksCollection", y = "missing")}: 
   plots sets of sets of tracks}
   \item{stplot}{\code{signature(obj = "TracksCollection")}: 
   plots sets of sets of tracks}
	% for spatial objects; does nothing but setting up a plotting region choosing
	% a suitable aspect if not given(see below), colouring the plot background using either a bg= argument or par("bg"), and possibly drawing axes.  }
	% \item{summary}{\code{signature(object = "Spatial")}: summarize object}
  }
}

\usage{
  Track(track, df = NULL, fn = TrackStats)
  Tracks(tracks, tracksData = data.frame(row.names=names(tracks)), 
  		fn = TrackSummary)
  TracksCollection(tracksCollection, tracksCollectionData,
  		fn = TracksSummary)
  TrackSummary(track)
  TracksSummary(tracksCollection)
  \S4method{[}{Track}(x, i, j, ..., drop = TRUE)
  \S4method{[}{TracksCollection}(x, i, j, ..., drop = TRUE)
  \S4method{coerce}{Track,data.frame}(from, to)
  \S4method{coerce}{Tracks,data.frame}(from, to)
  \S4method{coerce}{TracksCollection,data.frame}(from, to)
%  \S4method{over}{Tracks,Spatial}(x, y, returnList = FALSE, fn = NULL, ...)
%  \S4method{over}{TracksCollection,Spatial}(x, y, returnList = FALSE, 
}

\arguments{
\item{track}{object of class \link{STIDF-class}, representing a single trip}
\item{df}{optional \code{data.frame} with information between track points}
\item{tracks}{named list with \code{Track} objects}
\item{tracksData}{\code{data.frame} with summary data for each \code{Track}}
\item{tracksCollection}{list, with \code{Tracks} objects}
\item{tracksCollectionData}{data.frame, with summary data on
 \code{tracksCollection}}
\item{fn}{function; }
\item{x}{object of class \code{Track} etc}
%\item{y}{object of class \code{Spatial}}
\item{i}{selection of spatial entities}
\item{j}{selection of temporal entities (see syntax in package xts) }
\item{...}{selection of attribute(s)}
\item{drop}{logical}
\item{from}{from}
\item{to}{target class}
}

\value{Functions \code{Track}, \code{Tracks} and
\code{TracksCollection} are constructor functions that take the
slots as arguments, check object validity, and compute summary
statistics on the track and tracks sets.}

\author{ Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de} }
\references{ http://www.jstatsoft.org/v51/i07/ }

\note{\code{segments} is a \code{data.frame} form in which track
segments instead of track points form a record, with \code{x0},
\code{y0}, \code{x1} and \code{y1} the start and end coordinates}

\examples{
library(sp)
t0 = as.POSIXct(as.Date("2013-09-30",tz="CET"))
# person A, track 1:
x = c(7,6,5,5,4,3,3)
y = c(7,7,6,5,5,6,7)
n = length(x)
t = t0 + cumsum(runif(n) * 60)
require(rgdal)
crs = CRS("+proj=longlat")
#crs = CRS(as.character(NA))
stidf = STIDF(SpatialPoints(cbind(x,y),crs), t, data.frame(co2 = rnorm(n)))
A1 = Track(stidf)
# person A, track 2:
x = c(7,6,6,7,7)
y = c(6,5,4,4,3)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x,y),crs), t, data.frame(co2 = rnorm(n)))
A2 = Track(stidf)
# Tracks for person A:
A = Tracks(list(A1=A1,A2=A2))
# person B, track 1:
x = c(2,2,1,1,2,3)
y = c(5,4,3,2,2,3)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x,y),crs), t, data.frame(co2 = rnorm(n)))
B1 = Track(stidf)
# person B, track 2:
x = c(3,3,4,3,3,4)
y = c(5,4,3,2,1,1)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x,y),crs), t, data.frame(co2 = rnorm(n)))
B2 = Track(stidf)
# Tracks for person A:
B = Tracks(list(B1=B1,B2=B2))
Tr = TracksCollection(list(A=A,B=B))
stplot(Tr, scales = list(draw=TRUE))
stplot(Tr, attr = "direction", arrows=TRUE, lwd = 3, by = "direction")
stplot(Tr, attr = "direction", arrows=TRUE, lwd = 3, by = "IDs")
plot(Tr, col=2, axes=TRUE)
dim(Tr)
dim(Tr[2])
dim(Tr[2][1])
u = stack(Tr) # four IDs
dim(u)
dim(unstack(u, c(1,1,2,2))) # regroups to original
dim(unstack(u, c(1,1,2,3))) # regroups to three IDs
dim(unstack(u, c(1,2,2,1))) # regroups differently
as(Tr, "data.frame")[1:10,] # tracks separated by NA rows
as(Tr, "segments")[1:10,]   # track segments as records
}

\keyword{classes}

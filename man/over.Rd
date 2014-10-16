\name{over-methods}
\docType{methods}
\alias{over}
\alias{over,xts,xts-method}
\alias{over,ST,STS-method}
\alias{over,STF,STF-method}
\alias{over,STS,STF-method}
\alias{over,STI,STF-method}
\alias{over,STF,STS-method}
\alias{over,STS,STS-method}
\alias{over,STI,STS-method}
\alias{over,STF,STI-method}
\alias{over,STS,STI-method}
\alias{over,STI,STI-method}
\alias{over,STF,STFDF-method}
\alias{over,STS,STFDF-method}
\alias{over,STI,STFDF-method}
\alias{over,STF,STSDF-method}
\alias{over,STS,STSDF-method}
\alias{over,STI,STSDF-method}
\alias{over,STF,STIDF-method}
\alias{over,STS,STIDF-method}
\alias{over,STI,STIDF-method}
\alias{aggregate,ST-method}
\title{ consistent spatio-temporal overlay for objects inheriting from ST }
\description{ consistent spatio-temporal overlay for STF, STS and STI
objects, as well as their *DF counterpart: 
retrieves the indexes or attributes from one geometry at the spatio-temporal
points of another }
\usage{
\S4method{over}{STF,STF}(x, y, returnList = FALSE, fn = NULL, ...)
\S4method{over}{xts,xts}(x, y, returnList = FALSE, fn = NULL, ...)
\S4method{aggregate}{ST}(x, by, FUN = mean, \dots, simplify = TRUE)
}
\arguments{
\item{x}{geometry (S/T locations) of the queries}
\item{y}{layer from which the geometries or attributes are queried}
\item{returnList}{ logical; determines whether a list is returned, 
or an index vector }
\item{fn}{(optional) a function; see value }
\item{by}{ geometry over which attributes in \code{x} are aggregated
(this can be a \code{Spatial*} geometry, or a \code{ST*} geometry), or
temporal aggregation, such as "month", "10 minutes", or a function such
as \link[zoo]{as.yearmon}; see \link[zoo]{aggregate.zoo}. In case \code{x}
is of class \link{STFDF}, argument \code{by} may be "time" or "space", in
which cases aggregation over all time or all space is carried out.}
\item{FUN}{aggregation function }
\item{simplify}{boolean; if TRUE, and space or time dimensions can be
dropped, the simpler (\code{Spatial} or \code{xts}) object will be returned }
\item{...}{arguments passed on to function fn or FUN}
}
\value{ 
an object of length \code{length(x)}, or a data.frame with number
of rows equal to \code{length(x)}. If \code{returnList} is FALSE,
a vector with indices of \code{y} for each geometry (point, grid
cell centre, polygon or lines x time point) in \code{x}.  if \code{returnList} is
TRUE, a list of length \code{length(x)}, with list element \code{i}
the vector of indices of the geometries in \code{y} that correspond
to the $i$-th geometry in \code{x}.

Function aggregate.ST aggregates the attribute values of \code{x}
over the geometry (space, time, or space-time) of \code{by}, using
aggregation function FUN.

For the matching of time intervals, see \link{timeMatch}.

For setting, or retrieving whether time represents intervals, see
\link{timeIsInterval}.
} 
\section{Methods}{
\describe{
\item{x = "STF", y = "STF"}{ }
\item{x = "xts", y = "xts"}{ finds the row index of the instance
or interval of time instances of \code{x} matching to \code{y}. Only if
\code{timeIsInterval(x) == TRUE}, intervals are sought. In that case,
time intervals start at the time instance of a record, and end at
the next. The last time interval length is set to the interval length
of the one-but-last (non-zero) interval. In case of a single time
instance for \code{y}, its interval is right-open.  }

}
}

\note{
See also \link[sp]{over}; methods intersecting SpatialLines with
anything else, or SpatialPolygons with SpatialPolygons, need rgeos
to be loaded first.
}
\author{Edzer Pebesma, \email{edzer.pebesma@uni-muenster.de}}
\references{ http://www.jstatsoft.org/v51/i07/ }

\seealso{\link[sp]{over}; \code{vignette('sto')}, \code{vignette('over')}, 
\link{timeMatch}, \link{timeIsInterval}}

\keyword{methods}

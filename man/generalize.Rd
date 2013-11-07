\name{generalize}

\alias{generalize}
\alias{generalize,Track-method} 
\alias{generalize,Tracks-method} 
\alias{generalize,TracksCollection-method}  

\title{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}}

\description{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}.}

\usage{%
\S4method{generalize}{Track}(t, FUN = mean, ...,  timeInterval, distance, n, tol, toPoints)
\S4method{generalize}{Tracks}(t, FUN = mean, ...)
\S4method{generalize}{TracksCollection}(t, FUN = mean, ...)}

\arguments{
\item{t}{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}
\item{FUN}{The generalization method to be applied. Defaults to \code{mean} if none is passed.}
\item{timeInterval}{ (lower limit) time interval to split Track into segments }
\item{distance}{ (lower limit) distance to split Track into segments }
\item{n}{ number of points to form segments }
\item{tol}{ tolerance passed on to \link[rgeos]{gSimplify}, to generalize segments 
 using the Douglas-Peucker algorithm. }
\item{toPoints}{ keep mid point rather than forming \link[sp]{SpatialLines} segments }
\item{...}{Additional arguments passed to FUN}
}

\value{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}

\keyword{generalize}

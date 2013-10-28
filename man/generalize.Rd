\name{generalize}

\alias{generalize}
\alias{generalize,Track-method} 
\alias{generalize,Tracks-method} 
\alias{generalize,TracksCollection-method}  

\title{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}}

\description{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}.}

\usage{%
\S4method{generalize}{Track}(t, FUN = mean, ...)
\S4method{generalize}{Tracks}(t, FUN = mean, ...)
\S4method{generalize}{TracksCollection}(t, FUN = mean, ...)}

\arguments{%
\item{t}{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}
\item{FUN}{The generalization method to be applied. Defaults to \code{mean} if none is passed.}
\item{...}{Additional arguments. Please see the details section for further information.}}

\details{%
A criterion has to be passed by which is generalized. To date, \code{timeInterval} to split by time intervals, \code{distance} to split by distances and \code{n} to split by segments with each segment containing n points are supported. The method evaluates one criterion only (either time interval, distance, ...). A tolerance value \code{tol} can be passed to simplify the given geometry using the Douglas-Peucker algorithm.}

\value{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}

\keyword{generalize}
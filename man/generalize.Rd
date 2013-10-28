\name{generalize}

\alias{generalize}
\alias{generalize,Track-method} 
\alias{generalize,Tracks-method} 
\alias{generalize,TracksCollection-method}  

\title{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}}

\description{Generalize objects of class \code{Track}, \code{Tracks} and \code{TracksCollection}.}

\usage{%
\S4method{generalize}{Track}(t, timeInterval, FUN = mean)
\S4method{generalize}{Tracks}(t, timeInterval, FUN = mean)
\S4method{generalize}{TracksCollection}(t, timeInterval, FUN = mean)}

\arguments{%
\item{t}{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}
\item{timeInterval}{A time interval.}
\item{FUN}{A generalization method to be applied. Defaults to mean if none is passed.}}

\value{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}

\keyword{generalize}
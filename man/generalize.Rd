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
\item{FUN}{The generalization method to be applied. Defaults to mean if none is passed.}
\item{...}{A generalization criterion (timeInterval, distance or n).}}

\value{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}

\note{The method evaluates one generalization criterion only (either distance, time interval, ...).}

\keyword{generalize}
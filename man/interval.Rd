\name{timeIsInterval}
\alias{timeIsInterval}
\alias{timeIsInterval<-}
\alias{timeIsInterval,ANY-method}
\alias{timeIsInterval,ST-method}
\alias{timeIsInterval<-,ANY,logical-method}
\alias{timeIsInterval<-,ST,logical-method}
\alias{timeIsInterval<-,STT,logical-method}

\title{ retrieve, or set, information whether time reflects instance (FALSE) or intervals (TRUE) }
\description{ retrieve, or set, information whether time reflects instance (FALSE) or intervals (TRUE) }
\usage{ 
timeIsInterval(x, ...)
timeIsInterval(x) <- value
}
\arguments{
\item{x}{ object, of any class }
\item{...}{ ignored }
\item{value}{ logical; sets the timeIsInterval value}
}
\value{ logical; this function sets or retrieves the
attribute \code{timeIsInterval} of \code{x}, UNLESS \code{x}
is of class \code{ST}, in which case it sets or retrieves
this attribute for the \code{time} slot of the object, i.e.
\code{timeIsInterval(x@time) <- value}
}
\note{ From spacetime 0.8-0 on, timeIsInterval is dropped in favour of
a more generic time intervals by specifying \code{endTime} of each observation}
\seealso{\link{over}, \link{timeIsInterval}}

\keyword{dplot}

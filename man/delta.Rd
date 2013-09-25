\name{delta}
\alias{delta}
\title{ find default time interval end points when intervals are regular }
\description{ find default time interval end points when intervals are regular }
\usage{ 
delta(x)
}
\arguments{
\item{x}{ object of class \code{xts}, or of another class that can
be coerced into \code{POSIXct}}; 
}
\value{
sequence of \code{POSIXct} time stamps, indicating the end
of the time interval, given by the next observation in \code{x}.
The last interval gets the same width of the one-but-last interval.
}
\details{
to find the interval size for the last observation (which has no next
observation), \code{x} needs to be at least of length 2.
}
\author{Edzer Pebesma}
\references{ http://www.jstatsoft.org/v51/i07/ }
\examples{
x = as.POSIXct("2000-01-01") + (0:9) * 3600
delta(x)
}
\keyword{manip}

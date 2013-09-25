\name{timeMatch}
\alias{timeMatch}
\alias{timeMatch,ST,ST-method}
\alias{timeMatch,xts,xts-method}
\alias{timeMatch,POSIXct,POSIXct-method}
\alias{index}
\alias{MATCH}
\title{ match two (time) sequences }
\description{ match two (time) sequences, where each can
be intervals or instances. }
\usage{ 
timeMatch(x, y, returnList = FALSE, ...)
}
\arguments{
\item{x}{ ordered sequence, e.g. of time stamps }
\item{y}{ ordered sequence, e.g. of time stamps }
\item{returnList }{ boolean; should a list be returned with all matches (TRUE),
or a vector with single matches (FALSE)? }
\item{...}{ \code{end.x} and \code{end.y} can be specified for
\code{xts} and \code{POSIXct} methods}
}
\value{
if \code{returnList = FALSE}: integer vector of length
\code{length(x)} with indexes of \code{y} matching to each of
the elements of \code{x}, or NA if there is no match. See section
details for definition of match.

if \code{returnList = TRUE}: list of length \code{length(x)},
with each list element an integer vector with all the indexes
of \code{y} matching to that element of \code{x}.
}
\details{
When \code{x} and \code{y} are of class \code{xts} or \code{POSIXct},
\code{end.x} and \code{end.y} need to specify endpoint of intervals.

In case \code{x} and \code{y} are both not intervals, matching is
done on equality of values, using \link[base]{match}.

If \code{x} represents intervals, then the first interval is from
\code{x[1]} to \code{x[2]}, with \code{x[1]} included but \code{x[2]}
not (left-closed, right-open). In case of zero-width intervals
(e.g. \code{x[1]==x[2]}), nothing will match and a warning is raised.
Package \code{intervals} is used to check overlap of intervals,
using, \link[intervals]{interval_overlap}.
}
\seealso{\link{over}, \link{timeIsInterval}, \link[intervals]{interval_overlap}}
\author{Edzer Pebesma}
\references{ http://www.jstatsoft.org/v51/i07/ }
\examples{
t0 = as.POSIXct("1999-10-10")
x = t0 +c(0.5+c(2,2.1,4),5)*3600
y = t0 + 1:5 * 3600
x
y
#timeIsInterval(x) = FALSE
#timeIsInterval(y) = FALSE
timeMatch(x,y, returnList = FALSE)
timeMatch(x,y, returnList = TRUE)
#timeIsInterval(y) = TRUE
timeMatch(x,y, returnList = FALSE, end.y = delta(y))
timeMatch(x,y, returnList = TRUE, end.y = delta(y))
#timeIsInterval(x) = TRUE
timeMatch(x,y, returnList = FALSE, end.x = delta(x), end.y = delta(y))
timeMatch(x,y, returnList = TRUE, end.x = delta(x), end.y = delta(y))
#timeIsInterval(y) = FALSE
timeMatch(x,y, returnList = FALSE, end.x = delta(x))
timeMatch(x,y, returnList = TRUE, end.x = delta(x))

x = as.POSIXct("2000-01-01") + (0:9) * 3600
y = x + 1
y[1] = y[2]
x
y
TI = function(x, ti) { 
	timeIsInterval(x) = ti
	x 
}
#timeMatch(TI(y,FALSE),TI(y,FALSE))
#timeMatch(TI(y,TRUE), TI(y,TRUE))
#
#timeMatch(TI(x,FALSE),TI(y,FALSE))
#timeMatch(TI(x,FALSE),TI(y,TRUE))
#timeMatch(TI(x,TRUE), TI(y,FALSE))
#timeMatch(TI(x,TRUE), TI(y,TRUE))
#
#timeMatch(TI(x,FALSE),TI(y,FALSE), returnList = TRUE)
#timeMatch(TI(x,FALSE),TI(y,TRUE), returnList = TRUE)
#timeMatch(TI(x,TRUE), TI(y,FALSE), returnList = TRUE)
#timeMatch(TI(x,TRUE), TI(y,TRUE), returnList = TRUE)

}
\keyword{manip}

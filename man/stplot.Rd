\name{stplot}
\alias{stplot}
\alias{stplot.STFDF} 
\alias{stplot.STIDF} 
\alias{stplot,STFDF-method} 
\alias{stplot,STSDF-method} 
\alias{stplot,STIDF-method} 
\alias{stplot,STI-method} 
\alias{stplot,STT-method} 
\alias{stplot,STTDF-method} 
\alias{stplot,RasterStackBrick-method} 
\alias{stack.STFDF} 
\alias{stack.STSDF} 
\alias{stack.STIDF} 
\alias{segPanel}
\alias{tracksPanel}
\title{ produce trellis plot for STxDF object }
\description{ create trellis plot for ST objects }
\section{Methods}{
  \describe{
    \item{stplot}{\code{signature(x = "STFDF")}: plots object of class \link{STFDF}}
    \item{stplot}{\code{signature(x = "STSDF")}: plots object of class \link{STSDF}}
    \item{stplot}{\code{signature(x = "STI")}: plots object of class \link{STI}}
    \item{stplot}{\code{signature(x = "STIDF")}: plots object of class \link{STIDF}}
    \item{stplot}{\code{signature(x = "STT")}: plots object of class \link{STT}}
    \item{stplot}{\code{signature(x = "STTDF")}: plots object of class \link{STTDF}}
  }
}
\usage{ 
stplot(obj, ...)
stplot.STFDF(obj, names.attr = trimDates(obj), ...,
	as.table = TRUE, at, cuts = 15, scales = list(draw = FALSE), 
	animate = 0, mode = "xy", scaleX = 0, auto.key = list(space = key.space), 
	main, key.space = "right", type = "l", do.repeat = TRUE)
stplot.STIDF(obj, ..., names.attr = NULL, as.table = TRUE,
	scales = list(draw = FALSE), xlab = NULL, ylab = NULL,
	type = "p", number = 6, tcuts, sp.layout = NULL, xlim =
	bbox(obj)[1, ], ylim = bbox(obj)[2, ])

}
\arguments{
\item{obj}{ object of a class deriving from \code{ST}}
\item{names.attr}{ names that will be used in the strip; trimDates(obj)
trims "-01" ending(s) from printed Dates }
\item{as.table}{ logical; if TRUE, time will increas from top to bottom;
if FALSE, time will increase from bottom to top }
\item{at}{ values at which colours will change; see \link[lattice]{levelplot}}
\item{cuts}{ number of levels the range of the attribute would be divided into }
\item{animate}{ numeric; if larger than 0, the number of seconds between
subsequent animated time steps (loop; press ctrl-C or Esc to stop) }
\item{mode}{ plotting mode; if "xy", maps for time steps are plotted; 
if "xt", a space-time plot is constructed (see argument \code{scaleX},
but read details below); if "ts", multiple-locations time series
are plotted in a single plot, or in a separate panel for each
attribute; if "tp" single- or multi-attribute time series are
plotted in multiple panels, one panel per location. }
\item{scaleX}{ integer: 0, 1 or 2; when \code{mode} is "xt", used to determine
whether the index of the spatial location is shown (0), the x coordinate (1)
or the y coordinate (2). }
\item{auto.key}{see the \code{auto.key} argument in \link[lattice]{xyplot}}
\item{main}{character; plot title, use \code{NULL} to omit title}
\item{key.space}{character; see \link[lattice]{xyplot}}
\item{scales}{ scales drawing; see \code{scales} argument of \link[lattice]{xyplot}}
\item{xlab}{ x-axis label}
\item{ylab}{ y-axis label }
\item{type}{ character; use 'l' for lines, 'p' for symbols,
'b' for both lines and symbols }
\item{do.repeat}{ logical; repeat the animation in an infinite loop? }
\item{number}{ number of time intervals, equally spaced }
\item{tcuts}{time cuts in units of \code{index(obj)}; this overrides \code{number}}
\item{sp.layout}{list or NULL; see \link[sp]{spplot}}
\item{...}{ arguments passed on to \link[sp]{spplot} in case of 
plotting objects of class \code{STFDF} or \code{STIDF},
or to \link[lattice]{xyplot} in case of stplot.STIDF}
\item{xlim}{numeric, x range}
\item{ylim}{numeric, y range}
}
\value{ In non-animation and "xy" mode, \code{stplot} is a wrapper
around \link[sp]{spplot}, that automically plots each time stamp in
a panel.  The returned value is is a lattice plot.

In "xt" mode, a space-time plot with space on the x-axis and time
on the y-axis is plotted. By default, the space ID is plotted on the
x-axis, as space can be anything (points, polygons, grid cells etc).
When \code{scaleX} is set to 1 or 2, the x- resp. y-coordinates
of the spatial locations, obtained by \link[sp]{coordinates}, is
used instead. Beware: when the x-coordinate is plotted, and for
each (x,t) element multiple y-coordinates are sent to the plot,
it is not clear which (x,y,t) value becomes the plotted value,
so slicing single y values is adviced -- no checking is done.
The returned value is is a lattice plot.

In animation mode (\code{animate} > 0), single maps are animated
in an endless loop, with \code{animate} seconds between each. No
proper value is returned: the loop needs to be interrupted by
the user.
}
\references{ http://www.jstatsoft.org/v51/i07/ }
\note{vignette("spacetime") contains several examples}
\keyword{dplot}

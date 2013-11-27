\name{stcube}

\alias{stcube}
\alias{stcube,Track-method} 
\alias{stcube,Tracks-method}
\alias{stcube,TracksCollection-method}

\title{Draw a space-time cube.}

\description{Draw a space-time cube.}

\usage{%
\S4method{stcube}{Track}(x, xlab = "x", ylab = "y", zlab = "t", type = "l",
	aspect, xlim, ylim, zlim, showMap = FALSE, mapType = "osm", ..., y, z)
\S4method{stcube}{Tracks}(x, xlab = "x", ylab = "y", zlab = "t", type = "l",
	aspect, xlim, ylim, zlim, showMap = FALSE, mapType = "osm",
	normalizeBy = "week", ..., y, z, col)
\S4method{stcube}{TracksCollection}(x, xlab = "x", ylab = "y", zlab = "t",
	type = "l", aspect, xlim, ylim, zlim, showMap = FALSE, mapType = "osm",
	normalizeBy = "week", ..., y, z, col)}

\arguments{
\item{x}{An object of class \code{Track}, \code{Tracks} or \code{TracksCollection}.}
\item{xlab, ylab, zlab, type, aspect, xlim, ylim, zlim}{Arguments passed to plot3d() of package \code{rgl}.}
\item{showMap}{Flag if a basemap is to be shown on the xy plane.}
\item{mapType}{The tile server from which to get the map. Passed as \code{type} to openmap() of package \code{OpenStreetMap}.}
\item{normalizeBy}{An abstract time period (either \code{week} or \code{day}) to be normalized by.}
\item{y, z, col}{Ignored, but included in the method signature for implementation reasons.}
\item{...}{Additional arguments passed to plot3d() of package \code{rgl}.}}

\value{A space-time cube.}

\keyword{space-time cube}
# \subsection{Conversion from and to trip}

# Objects of class \code{trip} in package \pkg{trip} \citep{sumner},
# meant to represent trajectories, extend objects of class
# \code{SpatialPointsDataFrame} by indicating in which data value
# columns time and trip ID are, in slot \code{TOR.columns}. To not
# lose this information (in particular, which column contains the IDs),
# we will extend class \code{STIDF} to retain this info.

# The following example uses data from package \pkg{diveMove} \citep{luque}.
# It assumes that time in a trip object is ordered, as \pkg{xts}
# will order it otherwise.

# We first prepare the trip object:
# <<>>=
library(spacetime)
library(maptools)
library(diveMove)
library(trip)
data(sealLocs, package="diveMove")
sealLocs$time = as.POSIXct(sealLocs$time, tz="GMT") # not sure about tz!
ringy = subset(sealLocs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) = ringy[c("lon", "lat")]
tr = trip(ringy, c("time", "id"))
# @
# Next, we convert it into an \code{STTDF} object, and plot it:
# <<>>=
setAs("trip", "STTDF",
	function(from) {
		from$burst = from[[from@TOR.columns[2]]]
		time = from[[from@TOR.columns[1]]]
		rt = range(time)
		#timeIsInterval(rt) = timeIsInterval(time) = FALSE
		# TODO: take care of endTime?
		#from = from[order(time),]
		STIbox = STI(SpatialPoints(t(bbox(from))), rt)
		STT = new("STT", STIbox, traj = list(STI(geometry(from), time)))
		new("STTDF", STT, data = from@data)
	}
)
x = as(tr, "STTDF")
m = map2SpatialLines(map("world", 
	xlim = c(-100,-50), ylim = c(40,77), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
plot(m, axes=TRUE, cex.axis =.7)
lines(x, col = "red")
# @
# the resulting plot is shown in Figure~\ref{fig:trip}.

# \begin{figure}[htb]
# <<fig=TRUE,height=4,width=4,echo=FALSE>>=
plot(m, axes=TRUE, cex.axis =.7)
lines(x, col = "red")
# @
# \caption{ Trajectory, formed by satellite locations of a ringed
# seal caught and released in New York. }

# \label{fig:trip}
# \end{figure}

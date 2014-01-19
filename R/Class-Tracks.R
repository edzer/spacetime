# The first class is "Track", and contains a single track, or trip, followed by
# a person, animal or object. This means that consecutive location/time stamps
# are not interrupted by a period of substantially other activity. This object
# extends "STIDF", where locations and times as well as attributes measured at
# these locations and times, such as elevation, are stored. The function "Track"
# can now be used to build such an object from its components. The slot
# "connections" contains data about the segments between consecutive ST points.

setClass("Track",
  contains = "STIDF", # Locations, times and attribute data about the points.
  representation(connections = "data.frame"), 
  # With attribute data BETWEEN points: speed, direction etc.
  validity = function(object) {
	stopifnot(nrow(object@connections) + 1 == nrow(object@data))
    return(TRUE)
  }
)

directions_ll = function(cc, ll) {
	# cc a 2-column matrix with points, [x y] or [long lat]
	# ll a boolean, indicating longlat (TRUE) or not (FALSE)
	if (! ll) {
		dcc = apply(cc, 2, diff)
		((atan2(dcc[,1], dcc[,2]) / pi * 180) + 360) %% 360
	} else {
		# longlat:
		# http://www.movable-type.co.uk/scripts/latlong.html
		# initial bearing:
		cc = cc * pi / 180
		lat1 = head(cc[,2], -1)
		lat2 = tail(cc[,2], -1)
		lon1 = head(cc[,1], -1)
		lon2 = tail(cc[,1], -1)
		dlon = lon2 - lon1
		az = atan2(sin(dlon)*cos(lat2),
			cos(lat1)*sin(lat2)-sin(lat1)*cos(lat2)*cos(dlon))
		((az  / pi * 180) + 360) %% 360
	}
}

TrackStats = function(track) {
	if(class(track@sp)[1] == "SpatialPoints") {
		cc = coordinates(track@sp)
		ll = identical(is.projected(track), FALSE)
		distance = LineLength(cc, ll, FALSE)
		duration = diff(as.numeric(index(track@time))) # seconds
		if (any(duration == 0)) {
			print(track)
			stop("zero duration interval(s) detected")
		}
		speed = distance / duration # per second
		direction = directions_ll(cc, ll)
		df = data.frame(distance = distance, duration = duration, 
			speed = speed, direction = direction)
	} else {
		df = data.frame(matrix(nrow = length(track@sp) - 1, ncol = 0))
	}
}

# Computes segment lengths.

Track = function(track, df = NULL, fn = TrackStats) {
	if (!is.null(fn)) {
		stats = TrackStats(track)
		if (is.null(df))
			df = stats
		else {
			stopifnot(nrow(df) == nrow(stats))
			df = cbind(df, stats)
		}
	}
	new("Track", track, connections = df)
}

# A collection of Track objects for single ID (car, person etc.).

setClass("Tracks", 
	representation(tracks = "list", tracksData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracks, function(x) is(x, "Track"))))
		stopifnot(nrow(object@tracksData) == length(object@tracks))
		stopifnot(length(object@tracks) > 0)
		stopifnot(!is.null(names(object@tracks)))
		stopifnot(identicalCRS(object@tracks))
		return(TRUE)
	}
)

TrackSummary = function(track) {
	ix = index(track@time)
	bb = bbox(track@sp)
	conn = track@connections
	data.frame(
	xmin = bb[1,1],
	xmax = bb[1,2],
	ymin = bb[2,1],
	ymax = bb[2,2],
	tmin = min(ix),
	tmax = max(ix),
	n = length(track@sp),
	distance = sum(conn$distance),
	medspeed = quantile(conn$speed, 0.5)
	# TODO Compute some mean direction?
	)
}

# Pre-computes elements of tracksData.

Tracks = function(tracks, 
		tracksData = data.frame(row.names=names(tracks)), fn = TrackSummary) {
	if (is.null(names(tracks)))
		names(tracks) = paste("Track", 1:length(tracks), sep = "")
	new("Tracks", tracks = tracks, 
		tracksData = cbind(tracksData, do.call(rbind, lapply(tracks, fn))))
}

# Collection of Tracks for several IDs.
 
setClass("TracksCollection", 
	representation(tracksCollection = "list", 
		tracksCollectionData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracksCollection, class) == "Tracks"))
		stopifnot(length(object@tracksCollection) == 
			nrow(object@tracksCollectionData))
		stopifnot(length(object@tracksCollection) > 0)
		names = names(object@tracksCollection)
		stopifnot(!(is.null(names) || any(is.na(names))))
		stopifnot(identicalCRS(object@tracksCollection))
		return(TRUE)
	}
)

TracksSummary = function(tracksCollection) {
	tc = tracksCollection
	df = data.frame(n = sapply(tc, function(x) length(x@tracks)))
	df$xmin = sapply(tc, function(x) min(x@tracksData$xmin))
	df$xmax = sapply(tc, function(x) max(x@tracksData$xmax))
	df$ymin = sapply(tc, function(x) min(x@tracksData$ymin))
	df$ymax = sapply(tc, function(x) max(x@tracksData$ymax))
	df$tmin = sapply(tc, function(x) min(x@tracksData$tmin))
	df$tmin = do.call(c, 
		lapply(lapply(tc, function(x) x@tracksData$tmin), min))
	df$tmax = do.call(c, 
		lapply(lapply(tc, function(x) x@tracksData$tmax), max))
	row.names(df) = names(tracksCollection)
	df
}

TracksCollection = function(tracksCollection, tracksCollectionData = NULL,
	fn = TracksSummary) {
	if (is.null(names(tracksCollection)))
		names(tracksCollection) = paste("Tracks", 1:length(tracksCollection), 
		sep = "")
	if (is.null(tracksCollectionData))
		tracksCollectionData = TracksSummary(tracksCollection)
	new("TracksCollection", tracksCollection = tracksCollection, 
		tracksCollectionData = tracksCollectionData)
}

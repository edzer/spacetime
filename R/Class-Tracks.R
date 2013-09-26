#' The first class is `Track`, and contains a single track, or trip,
# followed by a person, animal or object. This means that consecutive
# location/time stamps are not interrupted by a period of substantially
# other activity.
#' This object extends `STIDF`, where locations and times as well as
# attributes measured at these locations and times, such as elevation,
# are stored. The function `Track` can now be used to build such an
# object from its components. The slot `connections contains data
# about the segments between consecutive ST points.
setClass("Track",
  contains = "STIDF", # locations, times and attribute data about the points
  representation(connections = "data.frame"), 
  # with attribute data BETWEEN points: speed, direction etc.
  validity = function(object) {
	stopifnot(nrow(object@connections) + 1 == nrow(object@data))
    return(TRUE)
  }
)

TrackStats = function(track) {
	cc = coordinates(track@sp)
	ll = is.projected(track@sp)
	distance = LineLength(cc, ifelse(is.na(ll), FALSE, ll), FALSE)
	speed = distance / diff(as.numeric(index(track@time))) # per second
	d = apply(cc, 2, diff)
	direction = atan(d[,2] / d[,1]) # radial
	df = data.frame(distance = distance, speed = speed, direction = direction)
}

Track = function(track, df = NULL, fn = TrackStats) { # computes segment lenghts
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

# a collection of Track objects for single ID (car, person etc)
setClass("Tracks", 
	representation(tracks = "list", tracksData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracks, function(x) is(x, "Track"))))
		stopifnot(nrow(object@tracksData) == length(object@tracks))
		stopifnot(length(object@tracks) > 0)
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
	# compute some mean direction?
	)
}
# pre-computes elements of tracksData:
Tracks = function(tracks, 
		tracksData = data.frame(row.names=names(tracks)), fn = TrackSummary) {
	new("Tracks", tracks = tracks, 
		tracksData = cbind(tracksData, do.call(rbind, lapply(tracks, fn))))
}

# collection of Tracks for several IDs 
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
	df$tmin = do.call(c, lapply(lapply(tc, function(x) x@tracksData$tmin), min))
	df$tmax = do.call(c, lapply(lapply(tc, function(x) x@tracksData$tmax), max))
	row.names(df) = names(tracksCollection)
	df
}

TracksCollection = function(tracksCollection, tracksCollectionData = NULL,
	fn = TracksSummary) {
	# TBD
	if (is.null(tracksCollectionData))
		tracksCollectionData = TracksSummary(tracksCollection)
	new("TracksCollection", tracksCollection = tracksCollection, 
		tracksCollectionData = tracksCollectionData)
}

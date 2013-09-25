#' The first class is `Track`, and contains a single track, or trip, followed by a person, animal or object. This means that consecutive location/time stamps are not interrupted by a period of substantially other activity.
## ------------------------------------------------------------------------
Track = setClass("Track",
  contains = "STIDF", # locations, times and attribute data about the points
  representation(connections = "data.frame"), # attribute data BETWEEN points: speed, direction etc.
  validity = function(object) {
	stopifnot(nrow(object@connections) + 1 == nrow(object@data))
    return(TRUE)
  }
)

#' This object extends `STIDF`, where locations and times as well as attributes measured at these locations and times, such as elevation, are stored. The function `Track` can now be used to build such an object from its components. The slot `connections contains data about the segments between consecutive ST points.
#' 
#' A further utility function, `CreateTrack` does a bit more work and computes segment lengths and speed, from an `STIDF` object, and puts it in the `connections` slot:
## ------------------------------------------------------------------------
CreateTrack = function(obj, df = NULL) { # computes segment lenghts
	cc = coordinates(obj@sp)
	ll = is.projected(obj@sp)
	if (is.na(ll))
		ll = FALSE
	L = LineLength(cc, sum = FALSE, longlat = ll)
	if (is.null(df))
		df = data.frame(lengths = lengths)
	else {
		stopifnot(nrow(df) == length(L))
		df$lengths = L
	}
	#df$speed = df$lengths / as.numeric(diff(index(obj@time)))
  df$speed = df$lengths / diff(as.numeric(index(obj@time)))
	Track(obj, connections = df)
}

#' 
#' 
#' The next class, `Tracks`, takes a collection (list) of `Track` objects, along with a metadata data.frame `tracksData` containing (summary) information, one record per `Track` (e.g. total length, duration, average speed). It is assumed that each `Tracks` object refers to a single person, animal or object.
## ------------------------------------------------------------------------
Tracks = setClass("Tracks", # a collection of Track objects for single ID (car, person etc)
	representation(tracks = "list", tracksData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracks, function(x) is(x, "Track"))))
		stopifnot(nrow(object@tracksData) == length(object@tracks))
		stopifnot(length(object@tracks) > 0)
		return(TRUE)
	}
)

CreateTracks = function(obj) {
	# pre-computes elements of tracksData
	df = data.frame(ID=1:length(obj))
	Tracks(obj, df)
}

#' 
#' 
#' `TracksCollection` finally collects `Tracks` collections for several persons, animals or objects, and has a slot `tracksCollectionData` with a summary record for each person/animal/object:
## ------------------------------------------------------------------------
TracksCollection = setClass("TracksCollection", # collection of Tracks for several IDs 
	representation(tracksCollection = "list", tracksCollectionData = "data.frame"),
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

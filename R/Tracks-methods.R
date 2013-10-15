# segments are data.frames with a segment on each row, with
# x0 y0 x1 y1 the first four values, followed by attributes.

setClass("segments", contains = "data.frame")

# Coerce to segments.

setAs("Track", "segments", 
	function(from) {
		cc = coordinates(from@sp)
		t = index(from@time)
		df = from@data
		data.frame(x0 = head(cc[,1], -1), y0 = head(cc[,2], -1),
			x1 = tail(cc[,1], -1), y1 = tail(cc[,2], -1),
			time = head(t, -1), head(df, -1), from@connections)
	}
)

setAs("Tracks", "segments", function(from) {
		ret = do.call(rbind, lapply(from@tracks, 
			function(x) as(x, "segments")))
		ret$Track = rep(names(from@tracks), 
			times = sapply(from@tracks, length) - 1)
		ret
	}
)

setAs("TracksCollection", "segments",
	function(from) {
		l = lapply(from@tracksCollection, function(x) as(x, "segments"))
		ret = do.call(rbind, l)
		ret$IDs = rep(names(from@tracksCollection), times = sapply(l, nrow))
		ret
	}
)

# Coerce to data frame.

setAs("Track", "data.frame", 
	function(from) as(as(from, "STIDF"), "data.frame")
)

setAs("Tracks", "data.frame", 
	function(from)
		do.call(rbind, lapply(from@tracks, 
			function(x) rbind(as(x, "data.frame"), NA)))
)

setAs("TracksCollection", "data.frame", 
	function(from) {
		l = lapply(from@tracksCollection, function(x) as(x, "data.frame"))
		n = sapply(l, nrow)
		ret = do.call(rbind, l)
		data.frame(ret, IDs = rep(names(from@tracksCollection), times = n))
	}
)

# Coerce to Line, Lines, SpatialLines and SpatialLinesDataFrame. 

setAs("Track", "Line", 
	function(from) Line(coordinates(from@sp))
)

setAs("Track", "Lines",
	function(from) Lines(list(as(from, "Line")), "ID")
)

setAs("Track", "SpatialLines",
	function(from) SpatialLines(list(as(from, "Lines")), CRS(proj4string(from)))
)

setAs("Tracks", "Lines", 
	function(from) {
		tz = from@tracks
		# The Lines ID is made up of the conjunction of the first and last 
		# Track ID, using hyphen as separator.
		Lines(lapply(tz, function(x) as(x, "Line")), 
			paste(names(tz)[1], names(tz)[length(tz)], sep = "-"))
	}
)

setAs("Tracks", "SpatialLines", 
	function(from) {
		l = lapply(from@tracks, function(x) as(x, "Lines"))
		for (i in seq_along(l))
			l[[i]]@ID = paste("ID", i, sep="")
		SpatialLines(l, CRS(proj4string(from)))
	}
)

setAs("Tracks", "SpatialLinesDataFrame", 
	function(from) SpatialLinesDataFrame(as(from, "SpatialLines"), 
		from@tracksData, match.ID = FALSE)
)

setAs("TracksCollection", "SpatialLines", 
	function(from) SpatialLines(lapply(from@tracksCollection, 
		function(x) as(x, "Lines")), CRS(proj4string(from)))
)

setAs("TracksCollection", "SpatialLinesDataFrame", 
	function(from) SpatialLinesDataFrame(as(from, "SpatialLines"),
		from@tracksCollectionData, match.ID = FALSE)
)

# Coerce to xts.

setAs("Track", "xts", 
	function(from)
		as(STIDF(sp = from@sp, time = from@time, data = from@data), "xts")
)

setAs("Tracks", "xts",
	function(from)
		do.call(rbind, lapply(from@tracks, function(x) as(x, "xts")))
)

setAs("TracksCollection", "xts",
	function(from)
		do.call(rbind, lapply(from@tracksCollection, function(x) as(x, "xts")))
)

# Coerce to STIDF.

setAs("Track", "STIDF", 
	function(from)
		STIDF(sp = from@sp, time = from@time, data = from@data)
)

setAs("Tracks", "STIDF",
	function(from)
		do.call(rbind, lapply(from@tracks, function(x) as(x, "STIDF")))
)

setAs("TracksCollection", "STIDF",
	function(from)
		do.call(rbind, lapply(from@tracksCollection, function(x) as(x, "STIDF")))
)

# Provide proj4string methods.

setMethod("proj4string", signature(obj = "Track"),
    function(obj) proj4string(obj@sp)
)
setMethod("proj4string", signature(obj = "Tracks"),
    function(obj) proj4string(obj@tracks[[1]])
)
setMethod("proj4string", signature(obj = "TracksCollection"),
    function(obj) proj4string(obj@tracksCollection[[1]])
)

# Provide plot methods.

setMethod("plot", "TracksCollection",
	function(x, y, ..., type = 'l', xlim = bbox(x)[,1],
			ylim = bbox(x)[,2], col = 1, lwd = 1, lty =
			1, axes = TRUE, Arrows = FALSE, Segments = FALSE, add = FALSE) {
		sp = x@tracksCollection[[1]]@tracks[[1]]@sp
		if (! add)
			plot(as(sp, "Spatial"), xlim = xlim, ylim = ylim, axes = axes, ...)
		if (axes == FALSE)
			box()
		if (Arrows || Segments) {
			df = as(x, "segments")
			args = list(x0 = df$x0, y0 = df$y0, x1 = df$x1, y1 = df$y1, 
				col = col, lwd = lwd, lty = lty, ...)
			if (Arrows)
				do.call(arrows, args)
			else
				do.call(segments, args)
		} else {
			df = as(x, "data.frame") 
			cn = coordnames(x)
			lines(df[[cn[1]]], df[[cn[2]]], col = col, 
				lwd = lwd, lty = lty, ...)
		}
	}
)

# Provide coordnames methods.

setMethod("coordnames", "Track", function(x) coordnames(x@sp))

setMethod("coordnames", "Tracks", function(x) coordnames(x@tracks[[1]]))

setMethod("coordnames", "TracksCollection",
	function(x) coordnames(x@tracksCollection[[1]])
)

# Provide bbox methods.

setMethod("bbox", "Track",
	function(obj) {
		bb = data.frame(t(bbox(obj@sp)))
		ix = index(obj@time)
		bb$time = c(min(ix), max(ix))
		rownames(bb) = c("min", "max")
		bb
	}
)

setMethod("bbox", "Tracks",
	function(obj) {
		df = obj@tracksData
		xr = c(min(df$xmin), max(df$xmax))
		yr = c(min(df$ymin), max(df$ymax))
		tr = c(min(df$tmin), max(df$tmax))
		cn = coordnames(obj)
		ret = data.frame(xr, yr, time = tr)
		colnames(ret)[1:2] = cn
		rownames(ret) = c("min", "max")
		ret
	}
)

setMethod("bbox", "TracksCollection",
	function(obj) {
		df = obj@tracksCollectionData
		xr = c(min(df$xmin), max(df$xmax))
		yr = c(min(df$ymin), max(df$ymax))
		tr = c(min(df$tmin), max(df$tmax))
		cn = coordnames(obj)
		ret = data.frame(xr, yr, time = tr)
		colnames(ret)[1:2] = cn
		rownames(ret) = c("min", "max")
		ret
	}
)

# Provide over methods.

setMethod("over", c("Track", "Spatial"),
	function(x, y, ...) {
		over(as(x, "SpatialLines"), y, ...)
	}
)

setMethod("over", c("Tracks", "Spatial"),
	function(x, y, ...) {
		over(as(x, "SpatialLines"), y, ...)
	}
)

setMethod("over", c("TracksCollection", "Spatial"),
	function(x, y, ...) {
		over(as(x, "SpatialLines"), y, ...)
	}
)

# Provide dimension methods.

dim.Track = function(x) c(points=length(x@sp))

dim.Tracks = function(x) c(tracks=length(x@tracks),
	points=sum(sapply(x@tracks,dim)))

dim.TracksCollection = function(x) c(IDs=length(x@tracksCollection),
	apply(sapply(x@tracksCollection,dim),1,sum))

# Provide selection methods.

subs.Tracks <- function(x, i, j, ... , drop = TRUE) {
	if (missing(i))
		i = 1:length(x@tracks)
	else if (is(i, "Spatial"))
		i = which(!is.na(over(x, geometry(i))))
	else if (is.logical(i))
		i = which(i)
	if (drop && length(i) == 1)
		x@tracks[[i]]
	else
		Tracks(x@tracks[i], x@tracksData[i,j,drop=FALSE])
}

setMethod("[", "Tracks", subs.Tracks)

subs.TracksCollection <- function(x, i, j, ... , drop = TRUE) {
	if (!missing(j))
		warning("second selection argument is ignored")
	if (missing(i))
		s = 1:length(x@tracksCollection)
	else if (is(i, "Spatial"))
		s = which(!is.na(over(x, geometry(i))))
	else if (is.logical(i))
		s = which(i)
	else if (is.list(i)) {
		stopifnot(all(sapply(i, function(element) is.numeric(element))))
		s = which(sapply(i, function(element) length(element) > 0))
		for(index in seq_along(s)) {
			tz = x@tracksCollection[[s[index]]]
			tz@tracks = tz@tracks[i[[s[index]]]]
			tz@tracksData = tz@tracksData[i[[s[index]]], ]
			# Write back the just processed Tracks element.
			x@tracksCollection[[s[index]]] = tz
		}
	}
	else
		s = i
	if (drop && length(s) == 1)
		if(is.list(i) && length(i[[s[1]]]) == 1)
			x@tracksCollection[[s]][i[[s[1]]]]
		else
			x@tracksCollection[[s]]
	else
		TracksCollection(x@tracksCollection[s], 
			x@tracksCollectionData[s,,drop=FALSE])
}

setMethod("[", "TracksCollection", subs.TracksCollection)

stack.TracksCollection = function (x, select, ...) {
	stopifnot(missing(select))
	splt = function(Tr) lapply(Tr@tracks, function(x) Tracks(list(x)))
	l = lapply(x@tracksCollection, function(x) splt(x))
	TracksCollection(do.call(c, l))
}

c.Tracks = function(...)
	Tracks(do.call(c, lapply(list(...), function(x) x@tracks)))

unstack.TracksCollection = function(x, form, ...) {
	TracksCollection(lapply(split(x@tracksCollection, form), 
		function(x) do.call(c, x)))
}

# Segments are data.frames with a segment on each row, with x0 y0 x1 y1 the
# first four values, followed by attributes.

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
		# The Lines ID is made up of the conjunction of the first and last Track
		# ID, using hyphen as separator.
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

setAs("Tracks", "xts",
	function(from)
		do.call(rbind, lapply(from@tracks, function(x) as(x, "xts")))
)

setAs("TracksCollection", "xts",
	function(from)
		do.call(rbind, lapply(from@tracksCollection, function(x) as(x, "xts")))
)

# Coerce to STIDF.

setAs("Tracks", "STIDF",
	function(from)
		do.call(rbind, lapply(from@tracks, function(x) as(x, "STIDF")))
)

setAs("TracksCollection", "STIDF",
	function(from)
		do.call(rbind, lapply(from@tracksCollection, 
			function(x) as(x, "STIDF")))
)

# Coerce to Spatial*

setAs("Track", "Spatial",
	function(from) {
		from@data$time = index(from@time)
		if (!all(from@data$time == from@endTime))
			from@data$endTime = from@endTime
		addAttrToGeom(from@sp, from@data, match.ID = FALSE)
	}
)

setAs("Tracks", "Spatial",
	function(from) {
		ret = do.call(rbind, lapply(from@tracks, 
			function(x) as(x, "Spatial")))
		ret$Track = rep(names(from@tracks), times = lapply(from@tracks, length))
		ret
	}
)

setAs("TracksCollection", "Spatial",
	function(from)
		do.call(rbind, lapply(from@tracksCollection, 
			function(x) as(x, "Spatial")))
)

# Coerce to SpatialPointsDataFrame.

setAs("Track", "SpatialPointsDataFrame", function(from) as(from, "Spatial"))
setAs("Tracks", "SpatialPointsDataFrame", function(from) as(from, "Spatial"))
setAs("TracksCollection", 
	"SpatialPointsDataFrame", function(from) as(from, "Spatial"))

# Provide coordinates methods.

setMethod("coordinates", "Track",
	function(obj) coordinates(obj@sp)
)

setMethod("coordinates", "Tracks",
	function(obj) do.call(rbind, lapply(obj@tracks,
		function(x) coordinates(x)))
)

setMethod("coordinates", "TracksCollection",
	function(obj) do.call(rbind, lapply(obj@tracksCollection,
		function(x) coordinates(x)))
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

# Provide plot methods. TODO Make more generic.

setMethod("plot", "TracksCollection",
	function(x, y, ..., type = 'l', xlim = stbox(x)[,1],
			ylim = stbox(x)[,2], col = 1, lwd = 1, lty =
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

# Provide stcube methods.

map3d = function(map, z, ...) {
	require(rgl)
	if(length(map$tiles) != 1)
		stop("Pass single map tile only.")
	nx = map$tiles[[1]]$xres
	ny = map$tiles[[1]]$yres
	xmin = map$tiles[[1]]$bbox$p1[1]
	xmax = map$tiles[[1]]$bbox$p2[1]
	ymin = map$tiles[[1]]$bbox$p1[2]
	ymax = map$tiles[[1]]$bbox$p2[2]
	xc = seq(xmin, xmax, len = ny)
	yc = seq(ymin, ymax, len = nx)
	col = matrix(data = map$tiles[[1]]$colorData, nrow = ny, ncol = nx)
	m = matrix(data = z, nrow = ny, ncol = nx)
	surface3d(x = xc, y = yc, z = m, col = col, ...)
}

normalize = function(time, by = "week", origin) {
	tn = as.numeric(time)
	if (by == "day")
		tn = (tn %% (3600 * 24)) / 3600 # decimal hours
	else if (by == "week")
		tn = (tn %% (3600 * 24 * 7)) / (3600 * 24) # decimal days
	else 
		stop(paste("unknown value for by: ",by))
	if (missing(origin))
		origin = as.POSIXct("1970-01-01")
	as.POSIXct(tn, origin = origin)
}

if(!isGeneric("stcube"))
	setGeneric("stcube", function(x, ...)
		standardGeneric("stcube"))

setMethod("stcube", signature(x = "Track"),
	function(x, xlab = "x", ylab = "y", zlab = "t", type = "l", aspect, xlim,
		ylim, zlim, showMap = FALSE, mapType = "osm", ..., y, z) {
		# "y" and "z" are ignored, but included in the method signature to avoid
		# passing them twice to plot3d().
		require(rgl)
		require(OpenStreetMap)
		coords = coordinates(x@sp)
		time = index(x@time)
		if(missing(aspect))
			aspect = if((asp = mapasp(x@sp)) == "iso") "iso" else c(1, asp, 1)
		if(missing(xlim))
			xlim = range(coords[, 1])
		if(missing(ylim))
			ylim = range(coords[, 2])
		if(missing(zlim))
			zlim = range(time)
		# If the basemap is to be shown, fetch map tile first to allow for
		# rendering everything in one go.
		if(showMap) {
			# Required by openmap().
			require(raster)
			map = openmap(upperLeft = c(ylim[2], xlim[1]),
				lowerRight = c(ylim[1], xlim[2]), type = mapType)
			map = openproj(x = map, projection = proj4string(x))
		}
		plot3d(x = coords[, 1], y = coords[, 2], z = time, xlab = xlab,
			ylab = ylab, zlab = zlab, type = type, aspect = aspect, xlim = xlim,
			ylim = ylim, zlim = zlim, ...)
		if(showMap)
			map3d(map = map, z = time[1])
	}
)

setMethod("stcube", signature(x = "Tracks"),
	function(x, xlab = "x", ylab = "y", zlab = "t", type = "l", aspect, xlim,
		ylim, zlim, showMap = FALSE, mapType = "osm", normalizeBy = "week", ..., y, z, col) {
		# "y", "z" and "col" are ignored, but included in the method signature
		# to avoid passing them twice to plot3d().
		require(rgl)
		require(OpenStreetMap)
		dim = dim(x@tracks[[1]])["geometries"]
		coordsAll = do.call(rbind, lapply(x@tracks, function(x) coordinates(x@sp)))
		timeAll = normalize(do.call(c, lapply(x@tracks,
			function(x) index(x@time))), normalizeBy)
		col = rainbow(length(x@tracks))
		if(missing(aspect))
			# mapasp() processes objects of class Spatial* only.
			aspect = if((asp = mapasp(as(x, "SpatialLines"))) == "iso") "iso" else c(1, asp, 1)
		if(missing(xlim))
			xlim = range(coordsAll[, 1])
		if(missing(ylim))
			ylim = range(coordsAll[, 2])
		if(missing(zlim))
			zlim = range(timeAll)
		# If the basemap is to be shown, fetch map tile first to allow for
		# rendering everything in one go.
		if(showMap) {
			# Required by openmap().
			require(raster)
			map = openmap(upperLeft = c(ylim[2], xlim[1]),
				lowerRight = c(ylim[1], xlim[2]), type = mapType)
			map = openproj(x = map, projection = proj4string(x))
		}
		plot3d(x = coordsAll[1:dim, 1], y = coordsAll[1:dim, 2],
			z = timeAll[1:dim], xlab = xlab, ylab = ylab, zlab = zlab,
			type = type, col = col[1], aspect = aspect, xlim = xlim,
			ylim = ylim, zlim = zlim, ...)
		tracks = x@tracks[-1]
		for(t in seq_along(tracks)) {
			coords = coordinates(tracks[[t]]@sp)
			time = normalize(index(tracks[[t]]@time), normalizeBy, timeAll[1])
			lines3d(x = coords[, 1], y = coords[, 2], z = time, col = col[t+1])
		}
		if(showMap)
			map3d(map = map, z = timeAll[1])
	}
)

setMethod("stcube", signature(x = "TracksCollection"),
	function(x, xlab = "x", ylab = "y", zlab = "t", type = "l", aspect, xlim,
		ylim, zlim, showMap = FALSE, mapType = "osm", normalizeBy = "week", ..., y, z, col) {
		# "y", "z" and "col" are ignored, but included in the method signature
		# to avoid passing them twice to plot3d().
		require(rgl)
		require(OpenStreetMap)
		dim = dim(x@tracksCollection[[1]]@tracks[[1]])["geometries"]
		coordsAll = do.call(rbind, lapply(x@tracksCollection,
			function(x) do.call(rbind, lapply(x@tracks, function(y) coordinates(y@sp)))))
		timeAll = normalize(do.call(c, lapply(x@tracksCollection,
			function(x) do.call(c, lapply(x@tracks,
				function(y) index(y@time))))), normalizeBy)
		col = rainbow(length(x@tracksCollection))
		if(missing(aspect))
			# mapasp() processes objects of class Spatial* only.
			aspect = if((asp = mapasp(as(x, "SpatialLines"))) == "iso") "iso" else c(1, asp, 1)
		if(missing(xlim))
			xlim = range(coordsAll[, 1])
		if(missing(ylim))
			ylim = range(coordsAll[, 2])
		if(missing(zlim))
			zlim = range(timeAll)
		# If the basemap is to be shown, fetch map tile first to allow for
		# rendering everything in one go.
		if(showMap) {
			# Required by openmap().
			require(raster)
			map = openmap(upperLeft = c(ylim[2], xlim[1]),
				lowerRight = c(ylim[1], xlim[2]), type = mapType)
			map = openproj(x = map, projection = proj4string(x))
		}
		plot3d(x = coordsAll[1:dim, 1], y = coordsAll[1:dim, 2],
			z = timeAll[1:dim], xlab = xlab, ylab = ylab, zlab = zlab,
			type = type, col = col[1], aspect = aspect, xlim = xlim,
			ylim = ylim, zlim = zlim, ...)
		for(tz in seq_along(x@tracksCollection)) {
			if(tz == 1)
				tracks = x@tracksCollection[[tz]]@tracks[-1]
			else
				tracks = x@tracksCollection[[tz]]@tracks
			for(t in seq_along(tracks)) {
				coords = coordinates(tracks[[t]]@sp)
				time = normalize(index(tracks[[t]]@time), normalizeBy, timeAll[1])
				lines3d(x = coords[, 1], y = coords[, 2], z = time, col = col[tz])
			}
		}
		if(showMap)
			map3d(map = map, z = timeAll[1])
	}
)

# Provide coordnames methods.

setMethod("coordnames", "Track", function(x) coordnames(x@sp))

setMethod("coordnames", "Tracks", function(x) coordnames(x@tracks[[1]]))

setMethod("coordnames", "TracksCollection",
	function(x) coordnames(x@tracksCollection[[1]])
)

# Provide stbox methods.

# AUTOMATIC: fallback to ST (see ST-methods.R)
#setMethod("stbox", "Track",
#	function(obj) {
#		bb = data.frame(t(bbox(obj@sp)))
#		bb$time = range(index(obj@time))
#		rownames(bb) = c("min", "max")
#		bb
#	}
#)

setMethod("stbox", "Tracks",
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

setMethod("stbox", "TracksCollection",
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

# Provide bbox methods.

setMethod("bbox", "Tracks", function(obj) t(stbox(obj)[1:2]))

setMethod("bbox", "TracksCollection", function(obj) t(stbox(obj)[1:2]))

# Provide generalize methods.

if(!isGeneric("generalize"))
	setGeneric("generalize", function(t, FUN = mean, ...)
		standardGeneric("generalize"))

setMethod("generalize", signature(t = "Track"),
	function(t, FUN = mean, ..., timeInterval, distance, n, tol, toPoints) {
		if (sum(!c(missing(timeInterval), missing(distance), missing(n))) != 1)
			stop("exactly one parameter from (timeInterval, distance, n) has to be specified")
		if(!missing(timeInterval)) {
			origin = index(t@time)
			cut = cut(origin, timeInterval)
			segmentLengths = rle(as.numeric(cut))$lengths
		} 
		if (!missing(distance)) {
			# Total distances from each point to the first one.
			origin = c(0, cumsum(t@connections$distance))
			cut = floor(origin / distance)
			segmentLengths = rle(cut)$lengths
		} 
		if (!missing(n)) {
			dim = dim(t)["geometries"]
			if(n != 1 && dim / n > 1) {
				rep = floor((dim-n)/(n-1) + 1)
				mod = (dim-n) %% (n-1)
				if(mod == 0)
					segmentLengths = rep(n, rep)
				else
					segmentLengths = c(rep(n, rep), mod + 1)
			} else
				segmentLengths = dim
		} 
		# Update segment lengths to consider all segments for generalisation. In
		# case the cut-point falls between two points of the track to be
		# generalised, attach the next point to the current segment. If the cut-
		# point matches a point of the track, leave everything as is.
		toIndex = cumsum(segmentLengths)
		segmentLengths_ = integer()
		for(i in seq_along(segmentLengths)) {
			if (i == length(segmentLengths)
				|| (!missing(timeInterval) && origin[toIndex[i]] %in% seq(origin[1], origin[length(origin)], timeInterval))
				|| (!missing(distance) && origin[toIndex[i]] > 0 && origin[toIndex[i]] %% distance == 0)
				|| (!missing(n)))
				segmentLengths_[i] = segmentLengths[i]
			else { 
				segmentLengths_[i] = segmentLengths[i] + 1
				if(i == length(segmentLengths) - 1 && segmentLengths[i+1] == 1)
					break()
			}
		}
		segmentLengths = segmentLengths_
		# Aggregate over each segment.
		stidfs = list()
		endTime = numeric(0)
		for(i in seq_along(segmentLengths)) {
			from = if(i == 1) 1 else tail(cumsum(segmentLengths[1:(i-1)]), n = 1) - (i-2)
			to = from + segmentLengths[i] - 1
			if(!missing(toPoints) && toPoints) {
				sp = t@sp[(from+to)/2]
			} else {
				l = Lines(list(Line(t@sp[from:to])), paste("L", i, sep = ""))
				sp = SpatialLines(list(l), proj4string = CRS(proj4string(t)))
				if(!missing(tol) && nrow(coordinates(sp)[[1]][[1]]) > 1)
					sp = gSimplify(spgeom = sp, tol = tol, topologyPreserve = TRUE)
			}
			time = t@time[from]
			endTime = if(length(endTime) == 0) t@endTime[to] else c(endTime, t@endTime[to])
			data = data.frame(lapply(t@data[from:to, , drop = FALSE], FUN, ...)) # EP added ...
			stidfs = c(stidfs, STIDF(sp, time, data))
		}
		stidf = do.call(rbind, stidfs)
		# Provide a workaround, since rbind'ing objects of class POSIXct as used
		# in the "endTime" slot of STIDF objects does not work properly.
		stidf@endTime = endTime
		Track(stidf)
	}
)

setMethod("generalize", signature(t = "Tracks"),
	function(t, FUN = mean, ...) {
		t@tracks = lapply(t@tracks,
			function(x) generalize(x, FUN, ...))
		t
	}
)

setMethod("generalize", signature(t = "TracksCollection"),
	function(t, FUN = mean, ...) {
		t@tracksCollection = lapply(t@tracksCollection,
			function(x) generalize(x, FUN, ...))
		t
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

# Provide aggregate methods.

setMethod("aggregate", "Track", 
	function(x, ...) {
		aggregate(as(x, "SpatialPointsDataFrame"), ...)
	}
)

setMethod("aggregate", "Tracks", 
	function(x, ...) {
		aggregate(as(x, "SpatialPointsDataFrame"), ...)
	}
)

setMethod("aggregate", "TracksCollection",
	function(x, ...) {
		aggregate(as(x, "SpatialPointsDataFrame"), ...)
	}
)

# Provide dimension methods.

dim.Track = function(x) c(geometries = length(x@sp))

dim.Tracks = function(x) c(tracks = length(x@tracks),
	geometries = sum(sapply(x@tracks, dim)))

dim.TracksCollection = function(x) c(IDs = length(x@tracksCollection),
	apply(sapply(x@tracksCollection,dim), 1, sum))

# Provide summary methods.

summary.Track = function(object, ...) {
	obj = list()
	obj$class = class(object)
	obj$dim = dim(object)
	obj$sp = summary(object@sp)
	obj$time = summary(object@time)
	obj$data = summary(object@data)
	obj$connections = summary(object@connections)
	class(obj) = "summary.Track"
	obj
}

setMethod("summary", "Track", summary.Track)

print.summary.Track = function(x, ...) {
	cat(paste("Object of class ", x$class, "\n", sep = ""))
	cat(" with Dimensions: (")
	cat(paste(x$dim, collapse = ", "))
	cat(")\n")
	cat("[[Spatial:]]\n")
	print(x$sp)
	cat("[[Temporal:]]\n")
	print(x$time)
	cat("[[Data attributes:]]\n")
	print(x$data)
	cat("[[Connections:]]\n")
	print(x$connections)
	invisible(x)
}

summary.Tracks = function(object, ...) {
	obj = list()
	obj$class = class(object)
	obj$dim = dim(object)
	obj$sp = summary(do.call(rbind, lapply(object@tracks, function(x) x@sp)))
	obj$time = summary(do.call(rbind, lapply(object@tracks, function(x) x@time)))
	obj$data = summary(do.call(rbind, lapply(object@tracks, function(x) x@data)))
	obj$connections = summary(do.call(rbind, lapply(object@tracks, function(x) x@connections)))
	class(obj) = "summary.Tracks"
	obj
}

setMethod("summary", "Tracks", summary.Tracks)

print.summary.Tracks = function(x, ...) {
	cat(paste("Object of class ", x$class, "\n", sep = ""))
	cat(" with Dimensions (tracks, geometries): (")
	cat(paste(x$dim, collapse = ", "))
	cat(")\n")
	cat("[[Spatial:]]\n")
	print(x$sp)
	cat("[[Temporal:]]\n")
	print(x$time)
	cat("[[Data attributes:]]\n")
	print(x$data)
	cat("[[Connections:]]\n")
	print(x$connections)
	invisible(x)
}

summary.TracksCollection = function(object, ...) {
	obj = list()
	obj$class = class(object)
	obj$dim = dim(object)
	obj$sp = summary(do.call(rbind, lapply(object@tracksCollection,
		function(x) do.call(rbind, lapply(x@tracks, function(y) y@sp)))))
	obj$time = summary(do.call(rbind, lapply(object@tracksCollection,
		function(x) do.call(rbind, lapply(x@tracks, function(y) y@time)))))
	obj$data = summary(do.call(rbind, lapply(object@tracksCollection,
		function(x) do.call(rbind, lapply(x@tracks, function(y) y@data)))))
	obj$connections = summary(do.call(rbind, lapply(object@tracksCollection,
		function(x) do.call(rbind, lapply(x@tracks, function(y) y@connections)))))
	class(obj) = "summary.TracksCollection"
	obj
}

setMethod("summary", "TracksCollection", summary.TracksCollection)

print.summary.TracksCollection = function(x, ...) {
	cat(paste("Object of class ", x$class, "\n", sep = ""))
	cat(" with Dimensions (IDs, tracks, geometries): (")
	cat(paste(x$dim, collapse = ", "))
	cat(")\n")
	cat("[[Spatial:]]\n")
	print(x$sp)
	cat("[[Temporal:]]\n")
	print(x$time)
	cat("[[Data attributes:]]\n")
	print(x$data)
	cat("[[Connections:]]\n")
	print(x$connections)
	invisible(x)
}

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
	if (!missing(j) && is.character(j)) {
		for(tz in seq_along(x@tracksCollection)) {
			for(t in seq_along(x[tz]@tracks)) {
				data = x[tz][t]@data
				connections = x[tz][t]@connections
				if(j %in% names(data))
					data = data[j]
				else
					# An empty data slot is returned if the passed attribute
					# does not exist. The same applies to the connections slot.
					data = data.frame(matrix(nrow = dim(x[tz][t])["geometries"], ncol = 0))
				if(j %in% names(connections))
					connections = connections[j]
				else
					connections = data.frame(matrix(nrow = dim(x[tz][t])["geometries"] - 1, ncol = 0))
				# Write back the just processed data and connection slots.
				x@tracksCollection[[tz]]@tracks[[t]]@data = data
				x@tracksCollection[[tz]]@tracks[[t]]@connections = connections
			}
		}
	}
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
	# Drop data structure. Only relevant in case one single Tracks/Track element
	# have/has been selected. Multiple Tracks elements are always returned as
	# TracksCollection, independently of whether drop is true or false.
	if (drop && length(s) == 1) {
		if(is.list(i) && length(i[[s[1]]]) == 1)
			# Last [] is 1, since all but one Track elements have been sorted
			# out above.
			x@tracksCollection[[s]][1]
		else
			x@tracksCollection[[s]]
	}
	# Retain data structure, even if only one single Tracks/Track element
	# have/has been selected.
	else
		TracksCollection(x@tracksCollection[s], 
			x@tracksCollectionData[s,,drop=FALSE])
}

setMethod("[", "TracksCollection", subs.TracksCollection)

setMethod("[[", c("Track", "ANY", "missing"), 
	function(x, i, j, ...) {
		# TODO What if the attribute name coexists in both the data and
		# connections slot? Returning a list is inconvenient in the way that it
		# raises new design issues when making selections on objects of class
		# Tracks or TracksCollection: How to merge lists if tracks differ in
		# their "attribute state" (the passed attribute coexists in both slots,
		# exists in one slot only, does not exist)?
		if(i %in% names(x@data))
			x@data[[i]]
		else
			# Returns NULL if the attribute does not exist.
			x@connections[[i]]
	}
)

setMethod("[[", c("Tracks", "ANY", "missing"),
	function(x, i, j, ...) {
		do.call(c, lapply(x@tracks, function(t) t[[i]]))
	}
)

setMethod("[[", c("TracksCollection", "ANY", "missing"),
	function(x, i, j, ...) {
		do.call(c, lapply(x@tracksCollection, function(t) t[[i]]))
	}
)

setReplaceMethod("[[", c("Track", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if(i %in% names(x@data))
			x@data[[i]] = value	
		else if(i %in% names(x@connections))
			x@connections[[i]] = value
		x 	
	}
)

setReplaceMethod("[[", c("Tracks", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		for(index in seq_along(x@tracks)) {
			if(i %in% names(x[index]@data)) {
				# "dim" (and with that "from" and "to") have to be reinitialized
				# each loop, since tracks might differ in their "attribute
				# state" (the passed attribute coexists in both slots, exists in
				# one slot only, does not exist).
				dim = sapply(x@tracks, function(t) dim(t)[["geometries"]])
				from = if(index == 1) 1 else cumsum(dim)[index-1] + 1
				to = cumsum(dim)[index]
				x@tracks[[index]]@data[[i]] = value[from:to]
			} else if(i %in% names(x[index]@connections)) {
				dim = sapply(x@tracks, function(t) dim(t)[["geometries"]]) - 1
				from = if(index == 1) 1 else cumsum(dim)[index-1] + 1
				to = cumsum(dim)[index]
				x@tracks[[index]]@connections[[i]] = value[from:to]
			}
		}
		x
	}
)

setReplaceMethod("[[", c("TracksCollection", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		index = 1
		for(tz in seq_along(x@tracksCollection)) {
			for(t in seq_along(x[tz]@tracks)) {
				if(i %in% names(x[tz][t]@data)) {
					dim = do.call(c, lapply(x@tracksCollection, 
						function(tz) sapply(tz@tracks,
							function(t) dim(t)[["geometries"]])))
					from = if(index == 1) 1 else cumsum(dim)[index-1] + 1
					to = cumsum(dim)[index]
					x@tracksCollection[[tz]]@tracks[[t]]@data[[i]] = value[from:to]
				} else if(i %in% names(x[tz][t]@connections)) {
					dim = do.call(c, lapply(x@tracksCollection, 
						function(tz) sapply(tz@tracks,
							function(t) dim(t)[["geometries"]]))) - 1
					from = if(index == 1) 1 else cumsum(dim)[index-1] + 1
					to = cumsum(dim)[index]
					x@tracksCollection[[tz]]@tracks[[t]]@connections[[i]] = value[from:to]
				}
				index = index + 1
			}
		}
		x
	}
)

setMethod("$", "Track", function(x, name) x[[name]])

setMethod("$", "Tracks", function(x, name) x[[name]])

setMethod("$", "TracksCollection", function(x, name) x[[name]])

setReplaceMethod("$", "Track", 
	function(x, name, value) {
		x[[name]] = value
		x 
	}
)

setReplaceMethod("$", "Tracks", 
	function(x, name, value) {
		x[[name]] = value
		x 
	}
)

setReplaceMethod("$", "TracksCollection", 
	function(x, name, value) {
		x[[name]] = value
		x 
	}
)

# Provide stack, unstack and concatenate methods.

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

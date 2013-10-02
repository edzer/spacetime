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

# segments are data.frames with a segment on each row, with
# x0 y0 x1 y1 the first four values, followed by attributes.

setClass("segments", contains = "data.frame")

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

setMethod("plot", "TracksCollection",
	function(x, y, ..., type = 'l', xlim = bbox(x)$x,
			ylim = bbox(x)$y, col = 1, lwd = 1, lty =
			1, axes = TRUE, Arrows = FALSE, Segments = FALSE) {
		sp = x@tracksCollection[[1]]@tracks[[1]]@sp
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

setMethod("coordnames", "Track", function(x) coordnames(x@sp))
setMethod("coordnames", "Tracks", function(x) coordnames(x@tracks[[1]]))
setMethod("coordnames", "TracksCollection",
	function(x) coordnames(x@tracksCollection[[1]])
)
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
dim.Track = function(x) c(points=length(x@sp))
dim.Tracks = function(x) c(tracks=length(x@tracks),
	points=sum(sapply(x@tracks,dim)))
dim.TracksCollection = function(x) c(IDs=length(x@tracksCollection),
	apply(sapply(x@tracksCollection,dim),1,sum))

subs.Tracks <- function(x, i, j, ... , drop = TRUE) {
	if (missing(i))
		i = 1:length(x@tracks)
	if (is(i, "Spatial"))
		i = which(!is.na(over(x, geometry(i))))
	if (is.logical(i))
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
	else
		s = i
	if (drop && length(s) == 1)
		x@tracksCollection[[s]]
	else
		TracksCollection(x@tracksCollection[i], 
			x@tracksCollectionData[i,,drop=FALSE])
}
setMethod("[", "TracksCollection", subs.TracksCollection)

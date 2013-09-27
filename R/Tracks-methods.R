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

Track2seg = function(obj) {
	stopifnot(is(obj, "Track"))
	cc = coordinates(obj@sp)
	t = index(obj@time)
	df = obj@data
	data.frame(x0 = head(cc[,1], -1), y0 = head(cc[,2], -1),
		x1 = tail(cc[,1], -1), y1 = tail(cc[,2], -1),
		time = head(t, -1), head(df, -1), obj@connections)
}

Tracks2seg = function(obj) {
	stopifnot(is(obj, "Tracks"))
	do.call(rbind, lapply(obj@tracks, Track2seg))
}

TracksCollection2seg = function(obj) {
	stopifnot(is(obj, "TracksCollection"))
	l = lapply(obj@tracksCollection, Tracks2seg)
	ret = do.call(rbind, l)
	ret$IDs = rep(names(obj@tracksCollection), times = sapply(l, nrow))
	ret
}

setMethod("plot", "TracksCollection",
	function(x, y, ..., type = 'l', xlim = bbox(x)$x,
			ylim = bbox(x)$y, col = 1, lwd = 1, lty =
			1, axes = TRUE) {
		sp = x@tracksCollection[[1]]@tracks[[1]]@sp
		plot(as(sp, "Spatial"), xlim = xlim, ylim = ylim, axes = axes, ...)
		if (axes == FALSE)
			box()
		df = as(x, "data.frame") 
		cn = coordnames(x)
		lines(df[[cn[1]]], df[[cn[2]]], col = col, lwd = lwd, lty = lty, ...)
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

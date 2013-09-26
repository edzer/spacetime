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

setMethod("plot", "TracksCollection", 
	function(x, y, ..., type = 'l', colorBy = "IDs") {
		df = as(x, "data.frame")
		if (colorBy == "IDs" && is.null(list(...)$col))
			col = as.numeric(as.factor(df$IDs))
		# print(col)
		cn = coordnames(x)
		f = as.formula(paste(cn[2], cn[1], sep = " ~ "))
		plot(f, df, asp = 1, type = type, col = col, ...)
	}
)

setMethod("coordnames", "Track",
	function(x) coordnames(x@sp)
)
setMethod("coordnames", "Tracks",
	function(x) coordnames(x@tracks[[1]])
)
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

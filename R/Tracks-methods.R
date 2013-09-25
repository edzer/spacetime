setAs("Track", "data.frame", function(from)
	as(as(from, "STIDF"), "data.frame")
)
setAs("Tracks", "data.frame", function(from)
	do.call(rbind, lapply(from@tracks, function(x) rbind(as(x, "data.frame"), NA)))
)
setAs("TracksCollection", "data.frame", function(from) {
		l = lapply(from@tracksCollection, function(x) rbind(as(x, "data.frame"), NA))
		n = sapply(l, nrow)
		ret = do.call(rbind, l)
		data.frame(ret, IDs = rep(names(from@tracksCollection), times = n))
	}
)

setMethod("plot", "TracksCollection", function(x, ..., type = 'l', colorBy = "IDs") {
		df = as(x, "data.frame")
		if (colorBy == "IDs")
			col = as.numeric(as.factor(df$IDs))
		# print(col)
		plot(lat ~ long, df, asp = 1, type = type, col = col, ...)
	}
)

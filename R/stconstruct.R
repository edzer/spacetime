stConstruct = function(x, space, time, SpatialObj = NULL, 
		TimeObj = NULL, crs = CRS(as.character(NA)), interval, endTime) {
	if (is(x, "xts")) {
		stopifnot(ncol(x) == length(space))
		return(STFDF(space, index(x), data.frame(x = as.vector(t(x)))))
	}
	if (is(x, "matrix"))
		x = data.frame(x)
	stopifnot(is(x, "data.frame"))
	#stopifnot(isIndex(space) && isIndex(time)) 
# old line:	if (length(space) > 1 && length(time) == 1) { 
  # BG: univariate time-wide tables get stuck here otherwise
	if (length(space) > 1 && length(time) == 1 && !is.list(time)) { # BG 
		# long format; space indicates columns with points, coordinates
			n = names(x)
		if (is.character(space))
			si = which(n %in% space)
		else
			si = space
		sp = SpatialPoints(x[si], crs)
		if (is.character(time))
			ti = which(n %in% time)
		else
			ti = time
		time = xts(matrix(1:nrow(x), dimnames=list(NULL, "timeIndex")), x[[ti]])
		if (!missing(interval) && isTRUE(interval))
			endTime = delta(time)
		else
			endTime = as.POSIXct(index(time))
		attr(endTime, "tzone") = attr(time, "tzone")
		return(STIDF(sp[time,], time, x[time, -c(si, ti), drop=FALSE], endTime))
	} else if (length(space) == 1 && length(time) == 1) {
		# long format, space indicates index of SpatialObj:
		stopifnot(!is.null(SpatialObj))
		# check every combination is there:
		if (any(table(x[,space], x[,time]) != 1)) { # NOT a full grid:
			# stop("space/time combinations not complete")
			# why not try Irregular?
			time = xts(matrix(1:nrow(x), dimnames=list(NULL, "timeIndex")), 
				x[,time])
			if (missing(endTime)) {
				endTime = index(time)
				if (!missing(interval) && interval)
					endTime = delta(time)
			}
			attr(endTime, "tzone") = attr(time, "tzone")
			return(STIDF(SpatialObj, time, x, endTime))
		} else {
			sut = sort(unique(x[,time]))
			tm = xts(matrix(1:length(sut), dimnames=list(NULL, "timeIndex")),
				sut)
			if (missing(endTime)) {
				endTime = delta(tm)
				if (!missing(interval) && !interval)
					endTime = as.POSIXct(index(tm))
			}
			attr(endTime, "tzone") = attr(tm, "tzone")
			sp = as.character(sort(unique(x[,space])))
			return(STFDF(SpatialObj[sp], tm, 
				x[order(x[,time],as.character(x[,space])),], endTime))
		}
	} else if (is.list(time)) {  
		# time-wide table; space coords or SpatialObj
		isIndex = function(x) is.character(x) || is.numeric(x)
		if (isIndex(space) && length(space) > 1)
			SpatialObj = SpatialPoints(x[,space], CRS)
		else if (missing(SpatialObj))
			SpatialObj = space
		xx = data.frame(lapply(time, function(g) stack(x[g])$values))
		if (missing(endTime)) {
			if (!missing(interval) && !interval)
				endTime = index(TimeObj)
			else
				endTime = delta(TimeObj)
			attr(endTime, "tzone") = attr(TimeObj, "tzone")
		}
		return(STFDF(SpatialObj, TimeObj, xx, endTime))
	} else if (is.list(space)) { 
		# space-wide table:
		xx = data.frame(lapply(space, function(g) as.vector(t(x[g]))))
		if (missing(endTime)) {
			endTime = delta(time)
			if (!missing(interval) && !interval)
				endTime = as.POSIXct(index(time))
		}
		return(STFDF(SpatialObj, time, 
			data.frame(values = as.vector(t(xx))), endTime))
	} 
	stop("unknown parameter combination")
}

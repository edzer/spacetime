STI = function(sp, time, endTime) {
	if (missing(endTime)) {
		if (is(time, "xts"))
			endTime = index(time)
		else 
			endTime = time
	}
	endTime = as.POSIXct(endTime)
	new("STI", ST(sp, time, endTime))
}

STIDF = function(sp, time, data, endTime) {
	if (missing(endTime)) {
		if (is(time, "xts"))
			endTime = index(time)
		else 
			endTime = time
	}
	endTime = as.POSIXct(endTime)
	if (!is(time, "xts")) {
		time0 = time
        time = xts(matrix(1:length(time), dimnames=list(NULL, "timeIndex")),
			time)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o,]
		endTime = endTime[o]
		data = data[o,,drop=FALSE]
		attr(endTime, "tzone") = attr(time, "tzone")
	}
	new("STIDF", STI(sp, time, endTime), data = data)
}

setMethod("coordinates", "STI", function(obj) {
		myCoordinates(obj@sp)
	}
)
index.STI = function(x, ...) {
	index(x@time)
}
index.STIDF = index.STI

as.data.frame.STI = function(x, row.names = NULL, ...) {
	timedata = x@time
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = row.names(x@sp),
		time = index(x),
		endTime = x@endTime,
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		ret = data.frame(ret, x@sp@data)
	ret
}
setAs("STI", "data.frame", function(from) as.data.frame.STI(from))

as.data.frame.STIDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STI"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STIDF", "data.frame", function(from) as.data.frame.STIDF(from))

as.xts.STIDF = function(x, ...) {
	ix = index(x@time)
	if (is(ix, "Date"))
		xts(x@data, index(x@time))
	else
		xts(x@data, index(x@time), tzone = attr(x@time, "tzone"))
}
setAs("STIDF", "xts", function(from) as.xts.STIDF(from))

subs.STIDF <- function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)
	missing.k = k = TRUE
	dots = list(...)
    if (length(dots) > 0) {
        missing.k = FALSE
        k = dots[[1]]
    }

	if (missing.i && missing.j && missing.k)
		return(x)

	# space
	if (missing.i)
		i = TRUE
	if (is(i, "Spatial"))
		i = !is.na(over(x@sp, geometry(i)))
	if (is.logical(i)) {
		i = rep(i, length.out = length(x@sp))
		i = which(i)
	} else if (is.character(i)) { # suggested by BG:
		i = match(i, row.names(x@sp), nomatch = FALSE)
	}

	# time
	if (missing.j)
		j = rep(TRUE, length=nrow(x@time))
	else {
		if (is.logical(j))
			j = which(j)
		t = xts(matrix(1:nrow(x@time), dimnames=list(NULL, "timeIndex")), 
				index(x@time))[j]
		j = as.vector(t[,1])
	}
	
	if (is.numeric(i) && is.numeric(j)) {
		i = 1:nrow(x@time) %in% i
		j = 1:nrow(x@time) %in% j
	}
	if (is.logical(i) && is.logical(j))
		i = i & j

	x@sp = x@sp[i,]
	x@time = x@time[i,]
	x@endTime = x@endTime[i]
	if (is(x, "STIDF"))
		x@data = x@data[i, k, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1) {
		if (is(x, "STIDF"))
			x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
		else
			x = x@sp
	}
	x
}
setMethod("[", "STIDF", subs.STIDF)
setMethod("[", "STI", subs.STIDF)

setMethod("addAttrToGeom", signature(x = "STI", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STIDF", x, data = y)
)

length.STI = function(x) { length(x@sp) }

length.STIDF = function(x) { length(x@sp) }

setMethod("geometry", "STIDF", function(obj) as(obj, "STI"))

rbind.STIDF <- function(...) {
    dots = list(...)
    names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	STIDF(
		sp =      do.call(rbind, lapply(dots, function(x) x@sp)),
		time =    do.call(c, lapply(dots, function(x) index(x))),
		data =    do.call(rbind, lapply(dots, function(x) x@data)),
		endTime = do.call(c, lapply(dots, function(x) x@endTime))
	) # will re-order according to time
}

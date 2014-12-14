#index <- function(x, ...) {
#	UseMethod("index")
#}

delta = function(x) {
	if (any(is.na(x)))
		stop("time values cannot be negative")
	augment.with.one = function(x) {
		ux = unique(x)
		l = length(ux)
		if (l <= 1)
			stop(
			"cannot derive time interval from length 1 or constant sequence")
		lx = length(x)
		x = x[c(1:lx,lx)]
		stopifnot(x[lx] == ux[l])
		x[lx+1] = x[lx] + (ux[l] - ux[l-1]) # + dt
		x
	}
	if (is(x, "xts"))
		x = index(x)
	ret = augment.with.one(as.POSIXct(x))[-1]
	ret
}

if (!isGeneric("timeMatch"))
	setGeneric("timeMatch", function(x, y, returnList = FALSE, ...)
		standardGeneric("timeMatch"))

setMethod(timeMatch, signature(x = "ST", y = "ST"),
	function(x, y, returnList = FALSE) {
		args = list(x = as.POSIXct(index(x@time)),
			y = as.POSIXct(index(y@time)))
		if (any(args$x != x@endTime))
			args$end.x = x@endTime
		if (any(args$y != y@endTime))
			args$end.y = y@endTime
		args$returnList = returnList
		do.call(timeMatch, args)
	}
)

setMethod(timeMatch, signature(x = "xts", y = "xts"),
	function(x, y, returnList = FALSE, end.x = NULL, end.y = NULL)
		timeMatch(as.POSIXct(index(x)), as.POSIXct(index(y)), 
			returnList, end.x, end.y)
)

timeMatchPOSIXct = function(x, y, returnList = FALSE, 
			end.x = NULL, end.y = NULL) {
	ti.x = !is.null(end.x) # x is interval
	ti.y = !is.null(end.y) # y is interval
	if (ti.x || ti.y || returnList) {
		#timeMatchIntervals(x, y, returnList, end.x, end.y)
		if (ti.x)
			x = Intervals(cbind(x, end.x), closed = c(TRUE, FALSE))
		else
			x = as.numeric(x)
		if (ti.y)
			y = Intervals(cbind(y, end.y), closed = c(TRUE, FALSE))
		else
			y = as.numeric(y)
		ret = interval_overlap(x, y)
		if (! returnList)
			ret = sapply(ret, function(x) x[1])
	} else
		ret = match(x, y)
	ret
}

setMethod(timeMatch, signature(x = "POSIXct", y = "POSIXct"), timeMatchPOSIXct)
setMethod(timeMatch, signature(x = "Date", y = "Date"), timeMatchPOSIXct)

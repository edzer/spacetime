if (!isGeneric("timeIsInterval"))
	setGeneric("timeIsInterval", function(x, ...)
		standardGeneric("timeIsInterval"))

if (!isGeneric("timeIsInterval<-"))
	setGeneric("timeIsInterval<-", function(x, value)
		standardGeneric("timeIsInterval<-"))

setMethod("timeIsInterval", "ST",
	function(x, ...) {
		any(as.POSIXct(index(x@time)) < x@endTime)
	}
)

setMethod("timeIsInterval", "ANY",
	function(x, ...) {
		stop("timeIsInterval is not supported any longer; time intervals are obtained by specifying endTime")
	}
)

setReplaceMethod("timeIsInterval", c("ST", "logical"),
	function(x, value) {
		if (isTRUE(value)) {
			if (min(diff(index(x@time))) <= 0)
				warning("zero-width time intervals may yield invalid matching results")
			x@endTime = delta(x@time)
		}
		x
	}
)

setReplaceMethod("timeIsInterval", c("ANY", "logical"),
	function(x, value) {
		stop("timeIsInterval<- is not supported any longer; time intervals are obtained by specifying endTime")
	}
)

if (!isClass("Intervals"))
	setClass("Intervals")

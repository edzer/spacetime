if (!isGeneric("endTime"))
	setGeneric("endTime", function(x, ...)
		standardGeneric("endTime"))

if (!isGeneric("endTime<-"))
	setGeneric("endTime<-", function(x, value)
		standardGeneric("endTime<-"))

setMethod("endTime", "ST", function(x, ...) x@endTime)

setReplaceMethod("endTime", c("ST", "POSIXct"), # including xts
	function(x, value) {
		stopifnot(length(value) == dim(x)[2])
		stopifnot(any(value < index(x@time)))
		x@endTime = value
		x
	}
)
setReplaceMethod("endTime", c("ST", "xts"), # including xts
	function(x, value) {
		endTime(x) = index(x)
		x
	}
)

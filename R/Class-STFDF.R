setClass("STF", # space-time full
  contains = "ST",
  validity = function(object) {
	time = as.POSIXct(index(object@time))
	endTime = object@endTime
	n = length(endTime)
  	stopifnot(all(time <= endTime))
	if (n > 1)
  		stopifnot(all(endTime[1:(n-1)] <= time[-1]))
    return(TRUE)
  }
)

setClass("STFDF", # space-time full data frame
  contains = "STF", 
  slots = c(data = "data.frame"),
  validity = function(object) {
    stopifnot(nrow(object@data) == length(object@sp) * nrow(object@time))
    .checkAttrIsUnique(object@sp, object@time, object@data)
    return(TRUE)
  }
)

setClass("ST",
  slots = c(sp = "Spatial", time = "xts", endTime = "POSIXct"),
  validity = function(object) {
    stopifnot(length(object@sp) >= 1)
	stopifnot(nrow(object@time) >= 1)
	stopifnot(nrow(object@time) == length(object@endTime))
	# do the time zones, if set, match?
	tz1 = attr(object@time, "tzone")
	tz2 = attr(object@endTime, "tzone")
	tz1.set = (!is.null(tz1) && !nchar(tz1)==0)
	tz2.set = (!is.null(tz2) && !nchar(tz2)==0)
	stopifnot(tz1.set == tz2.set)
	if (tz1.set)
		stopifnot(tz1 == tz2)
	if (any(names(object@time) %in% names(object@sp)))
		stop("name conflict: attribute names in sp and time slot must differ")
	return(TRUE)
  }
)

.checkAttrIsUnique = function(sp, time, data) {
	if (any(names(sp) %in% names(data)))
		stop("name conflict: attribute name(s) in data already present in sp slot of ST object")
	if (any(names(time) %in% names(data)))
		stop("name conflict: attribute name(s) in data already present in time slot of ST object")
}

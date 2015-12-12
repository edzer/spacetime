#.supportedTime = c("Date", "POSIXct", "timeDate", "yearmon", "yearqtr")

ST = function(sp, time, endTime) {
	if (!is(time, "xts")) {
		#stopifnot(is(time, .supportedTime))
		if (!timeBased(time))
			stop("time is not a time based class")
		t = 1:length(time)
		if (any(order(time, t) != t))
			stop("time must be ordered")
		time = xts(matrix(1:length(time), ncol = 1,
				dimnames = list(NULL, "timeIndex")), time)
	}
	if (any(is.na(index(time))))
		stop("NA time values not allowed")
	stopifnot(is(endTime, "POSIXct"))
	attr(endTime, "tzone") = attr(time, "tzone")
	if (any(is.na(endTime)))
		stop("NA endTime values not allowed")
	if (is(sp, "SpatialGrid")) {
		sp = as(sp, "SpatialPixels")
		warning("on constructing ST, converted SpatialGrid to SpatialPixels")
	}
	new("ST", sp = sp, time = time, endTime = endTime)
}

setMethod("[[", c("ST", "ANY", "missing"), 
	function(x, i, j, ...) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		x@data[[i]]
	}
)
setReplaceMethod("[[", c("ST", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [[<- method for object without attributes")
		if (is.character(i)) {
			if (any(!is.na(match(i, dimnames(coordinates(x@sp))[[2]]))))
				stop(paste(i, "is already present as a coordinate name!"))
			if (i == "time")
				stop("cannot set name to time")
		}
		x@data[[i]] <- value
    	.checkAttrIsUnique(x@sp, x@time, x@data)
		x
	}
)

setMethod("$", "ST", 
	function(x, name) {
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setReplaceMethod("$", "ST", 
	function(x, name, value) { 
		if (!("data" %in% slotNames(x)))
			stop("no $<- method for object without attributes")
		x@data[[name]] = value 
    	.checkAttrIsUnique(x@sp, x@time, x@data)
		x 
	}
)

dim.ST = function(x) {
	x = c(length(x@sp), nrow(x@time))
	names(x) = c("space", "time")
	x
}
dim.STxDF = function(x) {
	x = c(length(x@sp), nrow(x@time), ncol(x@data))
	names(x) = c("space", "time", "variables")
	x
}
dim.STF = dim.STS = dim.STI = dim.ST
dim.STFDF = dim.STSDF = dim.STIDF = dim.STxDF
dim.STTDF = function(x) {
	x = c(length(x@traj), sum(sapply(x@traj, length)), ncol(x@data))
	names(x) = c("trajectories", "points", "variables")
	x
}
dim.STT = function(x) {
	x = c(length(x@traj), sum(sapply(x@traj, length)))
	names(x) = c("trajectories", "points")
	x
}

if (!isGeneric("proj4string"))
	setGeneric("proj4string", function(obj)
		standardGeneric("proj4string"))
setMethod("proj4string", "ST", function(obj) proj4string(obj@sp))
if (!isGeneric("proj4string<-"))
	setGeneric("proj4string<-", function(obj, value)
		standardGeneric("proj4string<-"))
setReplaceMethod("proj4string", signature(obj = "ST", value = "ANY"), 
	function(obj,value) { proj4string(obj@sp) = value; obj })
if (!isGeneric("is.projected"))
	setGeneric("is.projected", function(obj)
		standardGeneric("is.projected"))
setMethod("is.projected", "ST", function(obj) is.projected(obj@sp))
if (!isGeneric("plot"))
	setGeneric("plot", function(x, y, ...)
		standardGeneric("plot"))

if (!isGeneric("spTransform"))
	setGeneric("spTransform", function(x, CRSobj, ...)
		standardGeneric("spTransform"))

if (!isGeneric("stbox"))
	setGeneric("stbox", function(obj)
		standardGeneric("stbox"))

setMethod("stbox", "ST", 
	function(obj) {
		bb = bbox(obj@sp)
		tr = range(index(obj@time))
		data.frame(t(bb), time = tr)
	}
)
setMethod("bbox", "ST", function(obj) t(stbox(obj)[1:2]))

spTransform.STT = function(x, CRSobj, ...) {
	if (!requireNamespace("rgdal", quietly = TRUE))
		stop("package rgdal required for spTransform.STT")
	x@traj = lapply(x@traj, spTransform, CRSobj)
	x@sp = spTransform(x@sp, CRSobj)
	x
}
setMethod("spTransform", signature("STT", "CRS"), spTransform.STT)

spTransform.ST = function(x, CRSobj, ...) {
	if (!requireNamespace("rgdal", quietly = TRUE))
		stop("package rgdal required for spTransform.ST")
	x@sp = spTransform(x@sp, CRSobj)
	x
}
setMethod("spTransform", signature("ST", "CRS"), spTransform.ST)

setMethod("geometry", "ST", function(obj) obj)

summary.ST = function(object, ...) {
    obj = list()
    obj[["class"]] = class(object)
    obj[["dim"]] = dim(object)
	obj[["sp"]] = summary(object@sp)
	obj[["time"]] = summary(object@time)
    if ("data" %in% slotNames(object))
		obj[["data"]] = summary(object@data)
    class(obj) = "summary.ST"
    obj
}
setMethod("summary", "ST", summary.ST)

print.summary.ST = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
	if (is(x, "STT"))
		cat(" with Dimensions (ntraj, total_points, attr): (")
	else
		cat(" with Dimensions (s, t, attr): (")
	cat(paste(x[["dim"]], collapse = ", "))
	cat(")\n")
	cat("[[Spatial:]]\n")
    print(x[["sp"]])
	cat("[[Temporal:]]\n")
    print(x[["time"]])
    if (!is.null(x$data)) {
        cat("[[Data attributes:]]\n")
        print(x$data)
    }
    invisible(x)
}

#asSpatialDataFrame = function(x) { # convert to lower
#	stopifnot(length(x@sp) == nrow(x@data))
#	if (is(x@sp, "SpatialPoints"))
#		return(SpatialPointsDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialLines"))
#		return(SpatialLinesDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialPixels"))
#		return(SpatialPixelsDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialGrid"))
#		return(SpatialGridDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialPolygons"))
#		return(SpatialPolygonsDataFrame(x@sp, x@data))
#	#if (is(x@sp, "SpatialRings"))
#	#	return(SpatialRingsDataFrame(x@sp, x@data))
#	stop("unknown Spatial class")
#}

#if (!isGeneric("aggregate"))
#	setGeneric("aggregate", function(x, ...)
#		standardGeneric("aggregate"))
if (!isGeneric("aggregateBy"))
	setGeneric("aggregateBy", function(x, by, FUN = mean, ...)
		standardGeneric("aggregateBy"))
if (!isGeneric("geometry"))
	setGeneric("geometry", function(obj)
		standardGeneric("geometry"))

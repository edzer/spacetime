aggregate_ST_temporal = function(x, by, FUN = mean, ..., simplify = TRUE) {
	stopifnot("data" %in% slotNames(x))
	FUN = match.fun(FUN)
	x = as(x, "STFDF")
	if (is.function(by))
		cc = by(index(x@time)) # time format index
	else if (is(by, "character")) { 
		ix = index(x@time)
		stopifnot(is(ix, c("Date", "POSIXt")))
		cc = cut(ix, by)
		if (is(ix, "Date"))
			cc = as.Date(cc)
		if (is(ix, "POSIXt"))
			cc = as.POSIXct(cc, tz = attr(ix, "tzone"))
	}
	d = vector("list", length = ncol(x@data))
	for (i in 1:length(d)) {
		# use aggregate.zoo, returns zoo object:
		agg = aggregate(as.zoo(as(x[,,i], "xts")), cc, FUN, ...)
		d[[i]] = as.vector(t(agg))
	}
	names(d) = names(x@data)
	d = as.data.frame(d)
	if (simplify && length(time(agg)) == 1) {
		if ("data" %in% slotNames(x@sp))
			d = data.frame(x@sp@data, d)
   		addAttrToGeom(geometry(x@sp), d, match.ID = FALSE)
	} else
		STFDF(x@sp, time(agg), d)
}

setMethod("aggregateBy", signature(x = "ST", by = "function"), 
	aggregate_ST_temporal)
setMethod("aggregateBy", signature(x = "ST", by = "character"), 
	aggregate_ST_temporal)

setMethod("aggregateBy", signature(x = "STFDF", by = "Spatial"),
	function(x, by, FUN = mean, ..., simplify = TRUE, 
			byTime = is(x, "STF") || is(x, "STS")) {
		stopifnot("data" %in% slotNames(x))
		FUN = match.fun(FUN)
		if (is(by, "SpatialGrid"))
			by = as(by, "SpatialPixels")
		if (byTime) {
			# aggregate over space areas, by time as of origin:
			ix = over(x@sp, geometry(by))
			sel = !is.na(ix)
			d = vector("list", length = ncol(x@data))
			for (i in 1:length(d)) {
				# use aggregate.zoo, returns zoo object:
				agg = aggregate(t(as(x[sel,,i], "xts")), list(ix[sel]), 
					FUN = FUN, ...)
				g = agg$Group.1 # first column
				d[[i]] = as.vector(as.matrix(agg[,-1])) # attributes, time-wide
			}
			names(d) = names(x@data)
			d = as.data.frame(d)
			if (simplify && length(by[g,]) == 1)
				xts(cbind(d, as.matrix(x@time)), index(x@time))
			else
				STFDF(by[g,], x@time, d)
		} else 
			aggregate(x, STF(by, range(index(x@time)))[,1],
				FUN = FUN, simplify = simplify, ...)
	}
)

aggregateBySTST = function(x, by, FUN = mean, ..., simplify = TRUE) {
	stopifnot("data" %in% slotNames(x))
	FUN = match.fun(FUN)
   	by0 = by
   	if (gridded(by@sp))
      	by@sp = as(by@sp, "SpatialPolygons")
   	df = over(by, x, fn = FUN, ...)
	if (simplify && length(by@sp) == 1) # return xts:
		xts(cbind(df, as.matrix(by@time)), index(by@time))
	else if (simplify && nrow(by@time) == 1) { # return spatial:
		if ("data" %in% slotNames(by0@sp))
			df = data.frame(df, by0@sp@data)
   		addAttrToGeom(geometry(by0@sp), df, match.ID = FALSE)
	} else { #  by0 is STx:
		if ("data" %in% slotNames(by0))
			df = data.frame(df, by0@data)
   		addAttrToGeom(by0, df, match.ID = FALSE)
	}
}
setMethod("aggregateBy", signature(x = "ST", by = "ST"),
	aggregateBySTST)

#setMethod("aggregate", signature(x = "ST"),
#	function(x, by, FUN = mean, ..., simplify = TRUE) 
#		# dispatches on "by" as well:
#		aggregateBy(x, by, FUN = FUN, simplify = simplify, ...)
#)
aggregate.ST = function(x, by, FUN, ..., simplify = TRUE)
	aggregateBy(x, by, FUN, simplify = simplify, ...)

aggregate.STFDF = function(x, by, FUN, ..., simplify = TRUE) {
	FUN = match.fun(FUN)
	if (identical(by, "time"))
		addAttrToGeom(x@sp,
			as.data.frame(apply(as.array(x), c(1,3), FUN, ...)),
			FALSE)
	else if (identical(by, "space"))
		xts(apply(as.array(x), c(2,3), FUN, ...), index(x@time))
	else
		aggregate.ST(x, by, FUN, ..., simplify = simplify)
}

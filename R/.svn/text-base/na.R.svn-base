# na methods from zoo

time.ST = function(x, ...) index(x@time)

start.ST = function(x, ...) start(x@time)

end.ST = function(x, ..., endTime=FALSE) {
	if (endTime)
		max(x@endTime)
	else
		end(x@time)
}

na.locf.STFDF = function (object, na.rm = FALSE, ...) {
	nv = dim(object)[3]
	df = vector("list", nv)
	names(df) = names(object@data)
	for (i in 1:nv) {
		x = na.locf(as(object[,,i], "xts"), na.rm = na.rm, ...)
		df[[i]] = as.vector(t(x))
	}
	STFDF(object@sp, index(x), data.frame(df))
}

na.spline.STFDF = function(object, x = time(object), 
		xout, ..., na.rm = TRUE) {
	nv = dim(object)[3]
	df = vector("list", nv)
	names(df) = names(object@data)
	for (i in 1:nv) {
		x = na.spline(as(object[,,i], "xts"), x, xout, na.rm = na.rm, ...)
		df[[i]] = as.vector(t(x))
	}
	STFDF(object@sp, index(x), data.frame(df))
}

na.approx.STFDF = function(object, x = time(object), 
		xout, ..., na.rm = TRUE) {
	nv = dim(object)[3]
	df = vector("list", nv)
	names(df) = names(object@data)
	for (i in 1:nv) {
		x = na.approx(as(object[,,i], "xts"), x, xout, na.rm = na.rm, ...)
		df[[i]] = as.vector(t(x))
	}
	STFDF(object@sp, index(x), data.frame(df))
}

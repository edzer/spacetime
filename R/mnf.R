MNF = function(Sigma.Noise, Sigma, x) {
	e = eigen(t(solve(t(Sigma), t(Sigma.Noise)))) # Sigma.Noise %*% Sigma^-1
	e$sdev = sqrt(e$values)
	e$rotation = e$vectors
	e$x = x %*% e$rotation
	colnames(e$x) = paste0("MNF", 1:ncol(e$x))
	e$scale = FALSE
	e$center = FALSE
	e$vectors = NULL
	class(e) = c("mnf", "prcomp")
	e
}

mnf = function(x, ...) UseMethod("mnf")

mnf.matrix = function(x, ..., Sigma.Noise, use = "complete.obs") {
	if (nrow(x) <= ncol(x)) # do diff
		warning("matrix rank deficient: covariance matrices will be singular")
	Sigma = cov(x, use = use)
	if (missing(Sigma.Noise))
		Sigma.Noise = 0.5 * cov(apply(x, 2, diff), use = use)
	MNF(Sigma.Noise, Sigma, x)
}

mnf.mts = function(x, ..., use = "complete.obs") {
	ret = NextMethod(as.matrix(x))
	attr(ret$x, "tsp") = attr(x, "tsp")
	class(ret$x) = class(x)
	ret
}

mnf.zoo = function(x, ..., use = "complete.obs") {
	ret = mnf(as.matrix(x), ..., use = use)
	ret$x = zoo(ret$x, index(x))
	ret
}

mnf.SpatialPixelsDataFrame = function(x, ..., use = "complete.obs") {
	mnf.SpatialGridDataFrame(x, ..., use = use) # gets $x right
}

mnf.SpatialGridDataFrame = function(x, ..., Sigma.Noise, use = "complete.obs") {
	if (missing(Sigma.Noise)) {
		get.dx = function(x) as.vector(x[-1,] - x[-nrow(x),]) # Delta_h
		get.dy = function(x) as.vector(x[,-1] - x[,-ncol(x)]) # Delta_v
		a = as(x, "array")
		cov.dx = 0.5 * cov(apply(a, 3, get.dx), ..., use = use)
		cov.dy = 0.5 * cov(apply(a, 3, get.dy), ..., use = use)
		Sigma.Noise = (cov.dx + cov.dy)/2.0
	}
	ret = MNF(Sigma.Noise, cov(x@data, ..., use = use), as.matrix(x@data))
	# put data into original structure:
	x@data = as.data.frame(ret$x)
	ret$x = x
	ret
}

mnf.RasterStack = function(x, ..., use = "complete.obs") {
	NextMethod(as(x, "SpatialGridDataFrame"))
}

mnf.RasterBrick = function(x, ..., use = "complete.obs") {
	NextMethod(as(x, "SpatialGridDataFrame"))
}

mnf.STSDF = function(x, ..., use = "complete.obs", mode = "temporal") {
	NextMethod(as(x, "STFDF"))
}

mnf.STFDF = function(x, ..., use = "complete.obs", mode = "temporal") {
	if (dim(x)[3] != 1)
		stop("select a single attribute")
	if (mode == "temporal") {
		ret = mnf(as(x, "zoo"), use = use, ...)
		x@data[[1]] = as.vector(t(ret$x))
		row.names(x@sp) = colnames(ret$x)
		ret$x = x
	} else if (mode == "spatial") {
		sp = as(x, "Spatial")
		stopifnot(gridded(sp))
		ret = mnf(sp, use = use, ...)
	} else
		stop("unknown mode")
	ret
}

predict.mnf = function(object, newdata, ...) {
	if (missing(newdata))
		object$x
	else
		scale(newdata, object$center, object$scale) %*% object$vectors
}

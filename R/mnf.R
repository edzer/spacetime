# > methods(class = "prcomp")
# [1] biplot  plot    predict print   summary


#' A package for computing maximum noise fraction (MNF) and min/max autocorrelation factors (MAF)
#'
#' The mnf package computers maximum noise fraction (MNF) and min/max autocorrelation factors (MAF)
#' 
#' @docType package
#' @import graphics stats utils zoo xts sp
#' @name mnf-package
NULL

#' Compute MNF factors from noise and observation covariance matrices
#'
#' @param Sigma.Noise covariance matrix of the noise process
#' @param Sigma covariance matrix of the observation process
#' @param x observations matrix
#' @return object of class \code{mnf}, containing eigenvalues (\code{values}) and eigenvectors (\code{vectors}) of \eqn{\Sigma_N \Sigma^{-1}}
#'
#' @name MNF1
#' @export
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

#' Generic mnf method
#'
#' generic to compute mnf from data
#'
#' @param x object for which an mnf method is available
#' @param ... ignored
#' @name mnf
#' @export
mnf = function(x, ...) UseMethod("mnf")

#' @param use method to deal with missing values when computing covariances; see \link{cov}
#'
#' @name mnf
#' @export
mnf.matrix = function(x, ..., use = "complete.obs") {
	if (nrow(x) <= ncol(x)) # do diff
		warning("matrix rank deficient: covariance matrices will be singular")
	Sigma = cov(x, use = use)
	Sigma.Noise = 0.5 * cov(apply(x, 2, diff), use = use)
	MNF(Sigma.Noise, Sigma, x)
}

mnf.mts = function(x, ..., use = "complete.obs") {
	ret = NextMethod(as.matrix(x))
	attr(ret$x, "tsp") = attr(x, "tsp")
	class(ret$x) = class(x)
	ret
}

#' @name mnf
#' @export
mnf.zoo = function(x, ..., use = "complete.obs") {
	ret = mnf(as.matrix(x), ..., use = use)
	ret$x = zoo(ret$x, index(x))
	ret
}

#' @name mnf
#' @export
mnf.SpatialPixelsDataFrame = function(x, ..., use = "complete.obs") {
	mnf.SpatialGridDataFrame(x, ..., use = use) # gets $x right
}

#' @name mnf
#' @export
mnf.SpatialGridDataFrame = function(x, ..., use = "complete.obs") {
	get.dx = function(x) as.vector(x[-1,] - x[-nrow(x),]) # Delta_h
	get.dy = function(x) as.vector(x[,-1] - x[,-ncol(x)]) # Delta_v
	a = as(x, "array")
	cov.dx = 0.5 * cov(apply(a, 3, get.dx), ..., use = use)
	cov.dy = 0.5 * cov(apply(a, 3, get.dy), ..., use = use)
	ret = MNF((cov.dx + cov.dy)/2.0, cov(x@data, ..., use = use), as.matrix(x@data))
	# put data into original structure:
	x@data = as.data.frame(ret$x)
	ret$x = x
	ret
}

#' @name mnf
#' @export
mnf.RasterStack = function(x, ..., use = "complete.obs") {
	NextMethod(as(x, "SpatialGridDataFrame"))
}

#' @name mnf
#' @export
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
		ret = mnf(as(x, "zoo"), use = use)
		x@data[[1]] = as.vector(t(ret$x))
		row.names(x@sp) = colnames(ret$x)
		ret$x = x
	} else if (mode == "spatial") {
		sp = as(x, "Spatial")
		stopifnot(gridded(sp))
		ret = mnf(sp, use = use)
	} else
		stop("unknown mode")
	ret
}

#' predict method for mnf objects
#'
#' predict factor scores from data and a mnf rotation factor
#'
#' @param object object of class \code{mnf}
#' @param newdata object (matrix) with new observations; if omitted, \code{object$x} will be used
#' @param ... ignored
#'
#' @method predict mnf
#' @export
predict.mnf = function(object, newdata, ...) {
	if (missing(newdata))
		object$x
	else
		scale(newdata, object$center, object$scale) %*% object$vectors
}

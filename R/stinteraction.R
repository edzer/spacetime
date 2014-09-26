stInteraction <- function(x) { 
	stopifnot(is(x, "STFDF"))
	for (i in 1:(dim(x)[3])) {
		y = as(x[,,i], "xts")
		x[[i]] = as.vector(t(y - (rowMeans(y) %o% colMeans(y))/mean(y)))
	}
	x
}

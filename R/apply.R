# or is this too trivial to provide?
STapply = function(X, MARGIN, FUN, ...) {
	stopifnot(class(X) == "STFDF")
	if (MARGIN == "space" || MARGIN == 1)
		FOREACHSPACEapply(X, FUN, ...)
	else if (MARGIN == "time" || MARGIN == 2)
		FOREACHTIMEapply(X, FUN, ...)
	else stop("MARGIN should be 1 (space) or 2 (time)")	
}

FOREACHSPACEapply = function(X, FUN, ...) {
	ret = lapply(1:length(X@sp), function(i) FUN(X[i,], ...))
	#STFDF(X@sp, ret[[1]], do.call(rbind, ret))
}

FOREACHTIMEapply = function(X, FUN, ...) {
	ret = lapply(1:nrow(X@time), function(i) FUN(X[,i], ...))
}

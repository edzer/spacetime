# copied from sp:
.index2list = function(x) {
	l = lapply(1:length(x), function(x) { integer(0) })
	notNA = !is.na(x)
	l[notNA] = x[notNA]
	l
}

over.xts = function(x, y, returnList = FALSE, fn = NULL, ...) {
	ix = index(x)
	iy = index(y)
	if (returnList) { # get all matches:
		tm = timeMatch(ix, iy, returnList = TRUE)
		stopifnot(is.null(fn))
		lapply(tm, function(P) { y[P, drop=FALSE] })
	} else { 
		tm = timeMatch(ix, iy, returnList = FALSE)
		if (is.null(fn)) { # get first match:
			if (any(is.na(tm))) { # will lead to error, because [.xts doesn't handle NA's
				nas = which(is.na(tm))
				tm[nas] = nas
				y[nas,] = NA
			}
			y[tm,]
		} else {
			l = lapply(tm, function(P) { apply(y[P, drop=FALSE], 2, fn, ...) })
			ret = do.call(rbind, l)
			xts(ret, ix)
		}
	}
}
setMethod("over", signature(x = "xts", y = "xts"), over.xts)

# y = STF:
over.STF.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
	if (returnList) {
    	space.index = over(x@sp, y@sp, returnList = TRUE)
		time.index = timeMatch(x, y, returnList = TRUE) 
		n = length(y@sp)
		lst = vector("list", length(space.index) * length(time.index))
		k = 1
		for (i in seq(along = time.index)) {
			for (j in seq(along = space.index)) {
				nj = length(space.index[[j]])
				if (length(time.index[[i]]) == 0 || 
						length(space.index[[j]]) == 0)
					lst[[k]] = integer(0)
				else
					lst[[k]] = rep((time.index[[i]] - 1) * n, each = nj) +
						space.index[[j]]
				k = k + 1
			}
		}
		lst
	} else {
    	space.index = over(x@sp, y@sp)
		time.index = rep(timeMatch(x, y), each = length(space.index))
		# compute the index of x in y as y is STF:
    	(time.index - 1) * length(y@sp) + space.index # space.index gets recycled
	}
}
setMethod("over", signature(x = "STF", y = "STF"), over.STF.STF)

over.STS.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
    space.index = over(x@sp, y@sp)[x@index[,1]]
	time.index = timeMatch(x, y)[x@index[,2]]
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index
	if (returnList)
		.index2list(idx)
	else
		idx
}
setMethod("over", signature(x = "STS", y = "STF"), over.STS.STF)

over.STI.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
    space.index = over(x@sp, geometry(y@sp))
	time.index = timeMatch(x, y, returnList = FALSE)
	# compute the index of x in y as y is STF:
    #idx = (unlist(time.index) - 1) * length(y@sp) + unlist(space.index)
    idx = (time.index - 1) * length(y@sp) + space.index
	if (returnList) 
		.index2list(idx)
	else
		idx
}
setMethod("over", signature(x = "STI", y = "STF"), over.STI.STF)

# y = STI:
over.STF.STI = function(x, y, returnList = FALSE, fn = NULL, ...)
	over(as(x, "STS"), y, returnList = returnList, fn = fn, ...)
setMethod("over", signature(x = "STF", y = "STI"), over.STF.STI)

over.STS.STI = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
	if (returnList) { # aggregate call:
		r = over(y, x, returnList = TRUE)
		sp:::.invert(r, length(r), length(x))
	} else
		over(as(x, "STI"), y, returnList = returnList, fn = NULL, ...) #improve?
}
setMethod("over", signature(x = "STS", y = "STI"), over.STS.STI)

over.STI.STI = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
	#if (returnList) warning("returnList not fully supported yet")
	lst = list(index(x@time), index(y@time), returnList = TRUE)
	if (any(x@endTime > as.POSIXct(index(x@time))))
		lst[["end.x"]] = x@endTime
	if (any(y@endTime > as.POSIXct(index(y@time))))
		lst[["end.y"]] = y@endTime
	#print(lst)
	time.index = do.call(timeMatch, lst)
	#print(time.index)
	ret = lapply(1:length(time.index), function(i) {
		ti = time.index[[i]] # the x[i] matching y entry indices
		if (length(ti) > 0)
			over(x@sp[i,], y@sp[ti,], returnList = TRUE)[[1]] + (ti - 1)
		else
			integer(0)
	})
	if (! returnList)
		unlist(lapply(ret, function(x) { x[1] }))
	else
		ret
}
setMethod("over", signature(x = "STI", y = "STI"), over.STI.STI)

# any ST x, but y = STS:
over.ST.STS = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(is.null(fn))
	ret = over(x, as(y, "STF"), returnList = returnList, fn = fn)
	if (!is.null(fn))
		return(ret)
	ix.sts = (y@index[,2] - 1) * length(y@sp) + y@index[,1]
	ix.stf = rep(as.integer(NA), nrow(y@time) * length(y@sp))
	ix.stf[ix.sts] = 1:nrow(y@index) # e.g. NA NA 1 NA 2 NA 3 4 5 NA 6 etc, sort of inverse of index
	if (returnList)
		lapply(ret, function(x) { sel = ix.stf[x]; sel[!is.na(sel)] })
	else
		ix.stf[ret]
}
setMethod("over", signature(x = "ST", y = "STS"), over.ST.STS)

overDFGenericST = function(x, y, returnList = FALSE, fn = NULL, ...) {
    stopifnot(identicalCRS(x, y))
	if (is.null(fn) && !returnList) {
    	r = over(x, geometry(y), returnList = FALSE)
		ret = y@data[r , , drop = FALSE]
	} else {
    	r = over(x, geometry(y), returnList = TRUE)
    	#ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
    	ret = overDF_for_rgeos(r, y@data, length(x), returnList, fn, ...)
	}
    if (!returnList)
        row.names(ret) = row.names(x)
    ret
}
setMethod("over", signature(x = "STF", y = "STFDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STFDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STFDF"), overDFGenericST)

setMethod("over", signature(x = "STF", y = "STSDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STSDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STSDF"), overDFGenericST)

setMethod("over", signature(x = "STF", y = "STIDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STIDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STIDF"), overDFGenericST)


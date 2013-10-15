STT = function(trajLst, STbox = NULL) {
	stopifnot(length(trajLst) > 0)
	if (is.null(names(trajLst)))
		names(trajLst) = 1:length(trajLst)
	if (is.null(STbox)) {
    	bb = sapply(trajLst, function(x) bbox(x@sp))
    	sp = SpatialPoints(rbind(c(min(bb[1,]), min(bb[2,])),
        	c(max(bb[3,]), max(bb[4,]))), trajLst[[1]]@sp@proj4string)
    	coordnames(sp) = coordnames(trajLst[[1]]@sp)
		tm = do.call(c, lapply(trajLst, index))
		rt = range(tm)
		STbox = STI(sp, rt)
	}
	new("STT", STbox, traj = trajLst)
}

STTDF = function(STT, data) {
	new("STTDF", STT, data = data)
}

index.STT = function(x, ...) {
	do.call(c, lapply(x@traj, index))
}
index.STTDF = index.STT

setMethod("geometry", "STTDF", function(obj) as(obj, "STT"))

setMethod("coordinates", "STT", function(obj)
		do.call(rbind, lapply(obj@traj, coordinates))
)

setMethod("addAttrToGeom", signature(x = "STT", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STTDF", x, data = y)
)

# plot.STTDF = function(x, y,..., byBurst = TRUE, 
# 			col = "black", lty = 1, lwd = 1, 
# 			type = "l", pch = 1,
# 			add = FALSE) {
# 	if (add == FALSE)
# 		plot(as(x@sp, "Spatial"), ...) # sets up plotting area
# 	if (byBurst)
# 		f = x$burst
# 	else {
# 		stopifnot(!is.null(x$id))
# 		f = x$id
# 	}
# 	lst = split(data.frame(coordinates(x)), f)
# 	col = rep(col, length = length(lst))
# 	lwd = rep(lwd, length = length(lst))
# 	lty = rep(lty, length = length(lst))
# 	for (i in seq(along = lst))
# 		lines(as.matrix(lst[[i]]), col = col[i], lty = lty[i], lwd = lwd[i],
# 			type = type, pch = pch)
# }
# 
# setMethod("plot", signature(x = "STTDF", y = "missing"), plot.STTDF)

subs.STTDF <- function(x, i, j, ... , useGeos = TRUE, drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)
	missing.k = k = TRUE
	dots = list(...)
    if (length(dots) > 0) {
        missing.k = FALSE
        k = dots[[1]]
    }

	if (missing.i && missing.j && missing.k)
		return(x)

	if (missing.i)
		i = TRUE
	
	l = lapply(x@traj, length) # lengths of trajectories

	if (is(i, "Spatial") || is(i, "ST")) {
		# select full trajectories:
		if (useGeos)
			i = !is.na(over(as(x, "SpatialLines"), geometry(i)))
		else
			i = sapply(x@traj, function(y) any(!is.na(over(y@sp, geometry(i)))))
	} 
	if (is.logical(i)) {
		i = rep(i, length.out = length(x@traj))
		i = which(i)
	} else if (is.character(i)) { # 
		# what if names are not present?
		i = match(i, names(x@trajLst), nomatch = FALSE)
	}

	# time
	if (missing.j)
		j = rep(TRUE, length=nrow(x@time))
	else {
		# defer to [.xts:
		tsel = xts(rep(1:length(l), l), index(x))[j,]
		j = unique(as.vector(tsel[,1]))
		i = i[i %in% j]
	}

	if (length(i) == 0)
		return(NULL)

	ret = STT(x@traj[i])
	
	if ("data" %in% slotNames(x)) { # STTDF
		sel = rep(1:length(l), l) %in% i
		ret = STTDF(ret, x@data[sel, k, drop = FALSE])
	}
	ret
}
setMethod("[", "STTDF", subs.STTDF)
setMethod("[", "STT", subs.STTDF)

length.STT = function(x) { length(x@traj) }
length.STTDF = function(x) { length(x@traj) }

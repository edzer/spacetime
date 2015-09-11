STI = function(sp, time, endTime) {
	if (missing(endTime)) {
		if (is(time, "xts"))
			endTime = index(time)
		else 
			endTime = time
	}
	endTime = as.POSIXct(endTime)
	new("STI", ST(sp, time, endTime))
}

STIDF = function(sp, time, data, endTime) {
	if (missing(endTime)) {
		if (is(time, "xts"))
			endTime = index(time)
		else 
			endTime = time
	}
	endTime = as.POSIXct(endTime)
	if (!is(time, "xts")) {
		time0 = time
        time = xts(matrix(1:length(time), dimnames=list(NULL, "timeIndex")),
			time)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o,]
		endTime = endTime[o]
		data = data[o,,drop=FALSE]
		attr(endTime, "tzone") = attr(time, "tzone")
	}
	new("STIDF", STI(sp, time, endTime), data = data)
}

setMethod("coordinates", "STI", function(obj) {
		myCoordinates(obj@sp)
	}
)
index.STI = function(x, ...) {
	index(x@time)
}
index.STIDF = index.STI

as.data.frame.STI = function(x, row.names = NULL, ...) {
	timedata = x@time
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = row.names(x@sp),
		time = index(x),
		endTime = x@endTime,
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		ret = data.frame(ret, x@sp@data)
	ret
}
setAs("STI", "data.frame", function(from) as.data.frame.STI(from))

as.data.frame.STIDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STI"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STIDF", "data.frame", function(from) as.data.frame.STIDF(from))

as.xts.STIDF = function(x, ...) {
	ix = index(x@time)
	if (is(ix, "Date"))
		xts(x@data, index(x@time))
	else
		xts(x@data, index(x@time), tzone = attr(x@time, "tzone"))
}
setAs("STIDF", "xts", function(from) as.xts.STIDF(from))

subs.STIDF <- function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)
	missing.k = k = TRUE
	dots = list(...)
  
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	}

	if(!missing.i) {
	  if(is.matrix(i))
	    i <- i[i[,1]==i[,2],1]
	  if(is.numeric(i))
  	  if(any(order(i)-1:length(i) != 0)) {
  	    warning("Spatial index has been sorted ascending.")
  	    i <- sort(i)
  	  }
	}
	
	if(!missing.j)
	  if(is.numeric(j))
  	  if(any(order(j)-1:length(j) != 0)) {
  	    warning("Temporal index has been sorted ascending.")
  	    j <- sort(j)
  	  }
	
	if (missing.i && missing.j && missing.k)
		return(x)
  
	if (!missing.i && is(i, "STI")) { # highjack i and j:
		j = which(!is.na(timeMatch(x,i)))
		i = which(!is.na(over(x@sp, geometry(i@sp))))
		missing.j = FALSE
	}

	matrix.i <- FALSE
  
	if (!missing.i) {
		if (is.matrix(i)) {
			stopifnot(ncol(i)==2)
			i <- i[order(i[,2]),,drop=FALSE]
			j <- i[,2]
			i <- i[,1]
			missing.j <- FALSE
			matrix.i <- TRUE
		}
	}

	# space
	if (missing.i)
		i = TRUE
	if (is(i, "Spatial"))
		i = !is.na(over(x@sp, geometry(i)))
	if (is.logical(i)) {
		i = rep(i, length.out = length(x@sp))
		i = which(i)
	} else if (is.character(i)) { # suggested by BG:
	  	if (length(i) > length(unique(i))) {
      		si <- numeric(0)
      		for (elem in i)
        		si <- c(si, which(row.names(x@sp) == elem))
      		i <- sort(si)
	  	} else
      		i = row.names(x@sp) %in% i
	}

	# time
	if (missing.j)
		j = rep(TRUE, length=nrow(x@time))
	else {
		if (is.logical(j))
			j = which(j)
		nct = ncol(x@time) # x <- rrSTI
		.time = cbind(x@time, 1:nrow(x@time))
		# uses [.xts, deals with character/iso8601;
		# takes care of negative indices:
		.time = .time[j]
		# get back the corresponding index vector t, to use for @data:
    	jtime <- as.vector(.time[, nct])
		j = as.vector(.time[, nct+1])
	}
	
	if (is.numeric(i) && is.numeric(j)) {
    	if(matrix.i)
      		i <- i[i==j]
    	else {
      		ui <- unique(i)
      		uj <- unique(j)
      		ti <- table(i)
      		tj <- table(j)
      
      		ind <- numeric(0)
      		for (elem in ui[ui %in% uj]) {
        		freq <- min(ti[names(ti) == as.character(elem)], 
                    tj[names(tj) == as.character(elem)])
        		ind <- c(ind, rep(elem, freq))
      		}
      		i <- ind[order(jtime[ind])]
    	}
	}

	if (is.logical(i) && is.logical(j))
		i = i & j

	x@sp = x@sp[i,]
	x@time = x@time[i,]
	x@endTime = x@endTime[i]
	if (is(x, "STIDF"))
		x@data = x@data[i, k, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1) {
		if (is(x, "STIDF"))
			x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
		else
			x = x@sp
	}
	x
}

setMethod("[", "STIDF", subs.STIDF)
setMethod("[", "STI", subs.STIDF)

setMethod("addAttrToGeom", signature(x = "STI", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STIDF", x, data = y)
)

length.STI = function(x) { length(x@sp) }

length.STIDF = function(x) { length(x@sp) }

setMethod("geometry", "STIDF", function(obj) as(obj, "STI"))

rbind.STIDF <- function(...) {
    dots = list(...)
    names(dots) <- NULL
	stopifnot(identicalCRS(dots))
	# c() drops tzone attribute:
	time =    do.call(c, lapply(dots, function(x) index(x)))
	attr(time, "tzone") = attr(index(dots[[1]]@time), "tzone")
	endTime = do.call(c, lapply(dots, function(x) x@endTime))
	attr(endTime, "tzone") = attr(index(dots[[1]]@endTime), "tzone")
	STIDF(
		sp =      do.call(rbind, lapply(dots, function(x) x@sp)),
		time =    time,
		data =    do.call(rbind, lapply(dots, function(x) x@data)),
		endTime = endTime
	) # will re-order according to time
}

STF = function(sp, time, endTime = delta(time)) {
	new("STF", ST(sp, time, endTime))
}

STFDF = function(sp, time, data, endTime = delta(time)) {
	new("STFDF", STF(sp, time, endTime), data = data)
}

myCoordinates = function(x) {
	stopifnot(is(x, "Spatial"))
	if (is(x, "SpatialLines"))
		do.call(rbind, lapply(coordinates(x), function(x) x[[1]][1,]))
	else
		coordinates(x)
}

setMethod("coordinates", "STF", function(obj) {
		mc = myCoordinates(obj@sp)
		m = matrix(apply(mc, 2, rep, nrow(obj@time)), ncol = ncol(mc))
		dimnames(m)[[2]] = coordnames(obj@sp)
		m
	})

index.STF = function(x, ...) {
	rep(index(x@time), each = length(x@sp))
}

index.STFDF = index.STF

as.data.frame.STF = function(x, row.names = NULL, ...) {
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
	timedata = apply(x@time, 2, rep, each = length(x@sp))
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = rep(factor(row.names(x@sp), levels = row.names(x@sp)),
			nrow(x@time)),
		time = index(x),
		endTime = rep(x@endTime, each= length(x@sp)),
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp)) {
		x = apply(x@sp@data, 2, rep, nrow(x@time))
		row.names(x) = NULL
		data.frame(ret, x)
	} else
		ret
}

setAs("STF", "data.frame", function(from) as.data.frame.STF(from))

as.data.frame.STFDF = function(x, row.names = NULL, ...) {
	f = as.data.frame(as(x, "STF"))
	data.frame(f, x@data, row.names = row.names, ...)
}

setAs("STFDF", "data.frame", function(from) as.data.frame.STFDF(from))

unstack.STFDF = function(x, form, which = 1,...) {
  if(missing(form))
#    form = as.formula(paste(names(x@data)[which], 
#      paste(coordnames(x@sp),collapse="+"), sep = "~"))
    form = as.formula(paste(names(x@data)[which], "sp.ID", sep = "~"))
  ret = unstack(as(x, "data.frame"), form, ...)
  rownames(ret) = make.unique(as.character(index(x@time)))
  ret
}

as.STFDF.xts = function(from) {
	nc = seq_along(from@data) # attribute columns indexes
	ret = do.call(cbind, lapply(nc, 
			function(i) {
				ix = index(from@time)
				if (is(ix, "Date"))
					xts(unstack(from[,,i, drop = FALSE]), ix)
				else
					xts(unstack(from[,,i, drop = FALSE]), ix, tzone = attr(from@time, "tzone"))
			}
		)
	)
	if (length(nc) > 1)
		names(ret) = as.vector(t(outer(names(from@data), row.names(from@sp), paste, sep = ".")))
	else
		names(ret) = row.names(from@sp)
	ret
}

setAs("STFDF", "xts", as.STFDF.xts)

as.zoo.STFDF = function(x,...) as.zoo(as(x, "xts"))

setAs("STFDF", "zoo", function(from) as.zoo.STFDF(from))

as.array.STFDF = function(x, ...) {
	a = array(NA, dim(x))
	for (i in 1:dim(x)[3])
		a[,,i] = t(as(x[,, i, drop = FALSE], "xts"))
	#dimnames(a) = list(names(x@sp), make.names(index(x@time)), names(x@data))
	dimnames(a) = list(NULL, make.names(index(x@time)), names(x@data))
	a
}

subs.STF_and_STFDF <- function(x, i, j, ... , drop = is(x, "STFDF")) {
	nr = dim(x)[1]
	nc = dim(x)[2]
	n.args = nargs()
	dots = list(...)
	missing.i = missing(i)
	missing.j = missing(j)
  
	# check wheter variables get selected
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	} else
		missing.k = TRUE
	
	if (missing.i && missing.j && missing.k)
		return(x)

	if (!missing.k && is(x, "STFDF")) {
		x@data = x@data[ , k, drop = FALSE]
		if (missing.j && n.args == 2)
			return(x)
	} 

	if (!missing.i && is(i, "STF")) { # highjack i and j:
		j = which(!is.na(timeMatch(x,i)))
		i = which(!is.na(over(x@sp, geometry(i@sp))))
		missing.j = FALSE
	}

	matrix.i <- FALSE
	
	# space
	########
  
	# keep track of original spatial indicies - if not yet 
	if (!is.character(row.names(x@sp)))
	  row.names(x@sp) <- as.character(row.names(x@sp))
  
	if (missing.i)
		s = 1:length(x@sp)
	else {
		if (is.matrix(i)) {
			stopifnot(ncol(i)==2)
			s <- unique(i[,1])
			missing.j <- FALSE
			matrix.i <- TRUE
		} else {
	 		if (is(i, "Spatial")) {
	 			s = which(!is.na(over(x@sp, geometry(i))))
	 		} else if (is.logical(i)) {
	 			i = rep(i, length.out = length(x@sp))
	  			s = which(i)
	 		} else if (is.character(i)) { # suggested by BG:
	 			s = match(i, row.names(x@sp), nomatch = FALSE)
	 		} else
	 			s = i
			# select geometry
			x@sp = x@sp[s,]
		}
	}
  
	# time
	#######
  
	if (missing.j)
		t = 1:nrow(x@time)
	else {
		if (matrix.i)
			t <- unique(i[,2])
		else {
			if (is.logical(j))
				j = which(j)
			nct = ncol(x@time)
	 		x@time = cbind(x@time, 1:nrow(x@time))
	 		# uses [.xts, deals with character/iso8601;
	 		# takes care of negative indices:
	 		x@time = x@time[j]
	 		# get back the corresponding index vector t, to use for @data:
	 		t = as.vector(x@time[, nct+1])
	 		x@time = x@time[,-(nct+1)]
	 		x@endTime = x@endTime[t]
		}
	}

	if (matrix.i) {
		ind <- i[order(i[,2]),]
    
		ind[,1] <- match(ind[,1], s)
		ind[,2] <- match(ind[,2], t)
		reOrder <- numeric(nrow(ind))
		for (ts in unique(ind[,2])) { # reordering data and index slot accoring to s
			selRow <- which(ind[,2] == ts)
			reOrder[selRow] <- selRow[order(ind[selRow,1])]
		}
    
		ind[,1] <- ind[reOrder,1]
    
		if (is(x, "STFDF")) {
			sel <- (i[,2]-1)*nr+i[,1]
			x <- STSDF(x@sp[s,], x@time[t,], x@data[sel,,drop=FALSE], ind, x@endTime[t])
		} else
			x <- STS(x@sp[s,], x@time[t,], ind, x@endTime[t])
	} else if (is(x, "STFDF"))
		x@data = data.frame(lapply(x@data, function(v) as.vector(matrix(v, nr, nc)[s,t])))
  
	# drop
	if (drop) {
		if (length(s) == 1 && all(s > 0)) { # space index has only 1 item:
			if (length(t) == 1) # drop time as well:
				x = x@data[1,]
			else {
				ix = index(x@time)
				xs = cbind(x@data, as.data.frame(x@time))
				if (is(ix, "Date"))
					x = xts(xs, ix)
				else
					x = xts(xs, ix, tzone = attr(x@time, "tzone"))
			}
		} else {
			if (length(t) == 1) {
				if(is(x, "STFDF")) # only one time step:
					x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
				else
					x = x@sp
			}
		} 
	}
	x
}

setMethod("[", "STFDF", subs.STF_and_STFDF)
setMethod("[", "STF", subs.STF_and_STFDF)

# provide a na.omit-method for STFDF objects
# removes rows and columns from the space-time grid
# containing NAs in the data
# Tom Gottfried
na.omit.STFDF <- function(object, drop=TRUE, ...){
	data <- na.omit(object@data)
	omit <- attr(data, "na.action")
	n <- length(object@sp)
	s <- unique((omit-1) %% n + 1)
	t <- unique((omit-1) %/% n + 1)
	if (drop && (length(s)==n || length(t)==nrow(object@time)))
		return(NA)
	else
#    return(object[-s,-t, drop=drop])
              # <= negative indices are partly not handled by [-method
#    return(object[(1:n)[!(1:n) %in% s],
#                  (1:nrow(object@time))[!1:nrow(object@time) %in% t],
#                  drop=drop])
              # <= logical indices partly not handled by [-method
		return(object[(1:n)[!(1:n) %in% s],
			(1:nrow(object@time))[!1:nrow(object@time) %in% t],
			drop=drop])
}

setMethod("addAttrToGeom", signature(x = "STF", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STFDF", x, data = y)
)

length.STF = function(x) { prod(dim(x)[1:2]) }

length.STFDF = function(x) { prod(dim(x)[1:2]) }

setMethod("geometry", "STFDF", function(obj) as(obj, "STF"))

nbMult = function(nb, st, addT = TRUE, addST = FALSE) {
	stopifnot(is(st, "STF"))
	stopifnot(is(nb, "nb"))
	stopifnot(length(nb) == length(st@sp))
	n = dim(st)[2] # time dimension
	if (n <= 1)
		return(nb)
	L = length(nb)
	ret = list()
	FN = function(x,i,j,L) {
		ret = as.integer(x + i * L) # spatial-only, for time i+1
		if (addT) {
			if (addST)
				now = c(ret, j + i * L)
			else
				now = j + i * L
			if (i > 0)
				ret = c(ret, now - L) # time-previous: j-iL
			if (i < (n-1))
				ret = c(ret, now + L) # time-next: j+iL
		}
		sort(ret)
	}
	for (i in 0:(n-1)) {
		app = lapply(1:L, function(j) FN(nb[[j]], i, j, L))
		ret = append(ret, app)
	}
	attributes(ret) = attributes(nb)
	attr(ret, "region.id") = as.character(1:length(ret))
	ret
}

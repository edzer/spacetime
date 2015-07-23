STS = function(sp, time, index, endTime = delta(time)) {
	new("STS", ST(sp, time, endTime), index = index)
}

STSDF = function(sp, time, data, index, endTime = delta(time)) {
	new("STSDF", STS(sp, time, index, endTime), data = data)
}

setMethod("coordinates", "STS", function(obj) {
		myCoordinates(obj@sp)[obj@index[,1],,drop=FALSE] # BG: added drop=FALSE to be consistent for single locations in space and time
	}
)

index.STS = function(x, ...) {
	index(x@time)[x@index[,2]]
}
index.STSDF = index.STS

as.data.frame.STS = function(x, row.names = NULL, ...) {
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
	timedata = as.data.frame(x@time)[x@index[,2], , drop = FALSE]
  	ret = data.frame(as.data.frame(coordinates(x)), 
                     sp.ID = row.names(x@sp)[x@index[,1]],
                     time = index(x),
                     endTime = x@endTime[x@index[,2]],
                     timedata, row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		data.frame(ret, x@sp@data[x@index[,1], , drop = FALSE])
	else
		ret
}
setAs("STS", "data.frame", function(from) as.data.frame.STS(from))

as.data.frame.STSDF = function(x, row.names = NULL, ...) {
	f = as.data.frame(as(x, "STS"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STSDF", "data.frame", function(from) as.data.frame.STSDF(from))
setAs("STSDF", "xts", function(from) as(as(from, "STFDF"), "xts")) # a good idea?

subs.STSDF <- function(x, i, j, ... , drop = is(x, "STSDF")) {
	n.args = nargs()
	dots = list(...)
	missing.i = missing(i)
	missing.j = missing(j)

  # check wheter variables get selected
	missing.k = TRUE
  	if (length(dots) > 0) {
    	missing.k <- FALSE
    	k <- dots[[1]]
  	}
  	if (!missing.i & !missing.j) {
    	if (is.matrix(i)) {
      	missing.k <- FALSE
      	k <- j
    	}
  	}
  
	if (missing.i && missing.j && missing.k)
		return(x)

	if (missing.k)
		k = TRUE
	else {
    	if (missing.j && n.args == 2) {
  			x@data = x@data[ , k, drop = FALSE]
  		return(x)
    	}
	} 

	if (!missing.i && is(i, "STS")) { # highjack i and j:
		j = which(!is.na(timeMatch(x,i)))
		i = which(!is.na(over(x@sp, geometry(i@sp))))
		missing.j = FALSE
	}

	matrix.i <- FALSE
  
	# space
	#######
  
	# keep track of original spatial indicies - if not yet 
  	if (!is.character(row.names(x@sp)))
    	row.names(x@sp) <- as.character(row.names(x@sp))
  
	if (missing.i)
		s = 1:length(x@sp)
  	else {
    	if (is.matrix(i)) { # BG
      		stopifnot(ncol(i)==2)
      		i <- i[order(i[,2]),,drop=FALSE]
      		s <- i[,1]
      		missing.j <- FALSE
      		matrix.i <- TRUE
    	} else {
      		if (is(i, "Spatial"))
  				s = which(!is.na(over(x@sp, geometry(i))))
  			else if (is.logical(i)) {
  				i = rep(i, length.out = length(x@sp))
  				s = which(i)
  			} else if (is.character(i)) # suggested by BG:
  				s = match(i, row.names(x@sp), nomatch = FALSE)
  			else 
  				s = i
    	}
	}

	# time
	if (missing.j)
		t = 1:nrow(x@time)
	else {
    	if (matrix.i)
      		t <- i[,2]
    	else {
  			if (is.logical(j))
  				j = which(j)
  			nct = ncol(x@time)
  			.time = cbind(x@time, 1:nrow(x@time))
  			# uses [.xts, deals with character/iso8601;
  			# takes care of negative indices:
  			.time = .time[j]
  			# get back the corresponding index vector t, to use for @data:
  			t = as.vector(.time[, nct+1])
    	}
	}

	si = x@index[,1] 
	ti = x@index[,2] 

  	if (matrix.i) # BG
    	sel <- unlist(sapply(1:nrow(i), function(x) which((si %in% s[x]) & (ti %in% t[x]))))
  	else {
    	if (length(unique(s)) < length(s) | length(unique(t)) < length(t)) {
      		sel <- numeric(0)
      		for (ts in t)
        		for (ss in s)
          			sel <- c(sel, which((si %in% ss) & (ti %in% ts)))
    	} else
      		sel = (si %in% s) & (ti %in% t)
  	}
  
	if (is(x, "STSDF"))
		x@data = x@data[sel, k, drop = FALSE]

  	# TG: Tom Gottfried reported at
  	# https://stat.ethz.ch/pipermail/r-sig-geo/2011-March/011231.html

	x@index = x@index[sel,, drop=FALSE]
  	# inserted drop=FALSE to handle (length(i)==1 && length(j)==1) # TG
	
  	# now simplify everything, and drop any S/T not refered to, but keep the ordering of "s":
	u1 = unique(s[s %in% x@index[,1]]) # was: unique(x@index[,1]))
  	u2 = unique(x@index[,2])
	x@sp = x@sp[u1,]
	x@time = x@time[u2,]
	x@endTime = x@endTime[u2]
	x@index[,1] <- match(x@index[,1], u1)
	x@index[,2] <- match(x@index[,2], u2)
  	reOrder <- numeric(nrow(x@index))
  	for (ts in unique(x@index[,2])) { # reordering data and index slot accoring to s
    	selRow <- which(x@index[,2] == ts)
    	reOrder[selRow] <- selRow[order(x@index[selRow,1])]
  	}

  	x@index[,1] <- x@index[reOrder,1]
  	if (is(x, "STSDF"))
    	x@data = x@data[reOrder, ,drop=FALSE]
  
	if (drop) {
		if (length(s) == 1) { # space index has only 1 item:
			if (length(t) == 1)
				x = x@data[1, , drop = (ncol(x@data) == 1)] # TRUE would return a list if ncol > 1
			else {
				ix = index(x@time[x@index[,2]])
				xs = cbind(x@data, as.data.frame(x@time)) # JS
				if (is(ix, "Date"))
					x = xts(xs, ix)
				else
					x = xts(xs, ix, tzone = attr(x@time, "tzone"))
        # added index to achieve 
				# (nrow(x)==length(order.by)) in index() # TG
			}
		} else {
      		if (length(t) == 1) { # only one time item
  				if (is(x, "STSDF")) {
  					sp = x@sp[x@index[,1],]
					if ("data" %in% slotNames(sp))
						x@data = cbind(x@data, sp@data) # otherwise they'd get lost
  					x = addAttrToGeom(sp, x@data, match.ID = FALSE)
              	# added index to achieve matching SpatialPoints and data.frame # TG
  				} else
  					x = x@sp[x@index[,1],]
  			}
		}
	}
	x
}

setMethod("[", "STSDF", subs.STSDF)
setMethod("[", "STS", subs.STSDF)

setMethod("addAttrToGeom", signature(x = "STS", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STSDF", x, data = y)
)

length.STS = function(x) { nrow(x@index) }

length.STSDF = function(x) { nrow(x@index) }

setMethod("geometry", "STSDF", function(obj) as(obj, "STS"))

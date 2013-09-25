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
	timedata = x@time[x@index[,2],]
  	ret = data.frame(as.data.frame(coordinates(x)), 
                     sp.ID = row.names(x@sp)[x@index[,1]],
                     time = index(x),
                     endTime = x@endTime[x@index[,2]],
                     timedata, row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		ret = data.frame(ret, x@sp@data[x@index[,1],,drop=FALSE])
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
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	} else
		missing.k = TRUE
	if (missing.i && missing.j && missing.k)
		return(x)

	if (missing.k) {
		k = TRUE
	} else if (missing.j && n.args == 2) {
		x@data = x@data[ , k, drop = FALSE]
		return(x)
	} 

	# space
	if (missing.i)
		s = 1:length(x@sp)
	else {
		if (is(i, "Spatial"))
			s = which(!is.na(over(x@sp, geometry(i))))
		else if (is.logical(i)) {
			i = rep(i, length.out = length(x@sp))
			s = which(i)
		} else if (is.character(i)) { # suggested by BG:
			s = match(i, row.names(x@sp), nomatch = FALSE)
		} else
			s = i
	}

	# time
	if (missing.j)
		t = 1:nrow(x@time)
	else {
		if (is.logical(j))
			j = which(j)
		.time = xts(matrix(1:nrow(x@time), dimnames=list(NULL, "timeIndex")),
			index(x@time), tzone = attr(x@time, "tzone"))
		# the following uses [.xts, deals with character/iso8601,
		# and takes care of negative indices:
		.time = .time[j] 
		# retrieve the corresponding index vector t, to use for @data:
		t = as.vector(.time[,1])
	}

	si = x@index[,1] 
	  # instead of: si = rep(1:length(x@sp), nrow(x@time)) # BG
	ti = x@index[,2] 
	  # instead of: ti = rep(1:nrow(x@time), each = length(x@sp)) # BG
	#x@sp = x@sp[s,] -- time and space topology not touched
	#x@time = x@time[t]
	sel = (si %in% s) & (ti %in% t)
	if (is(x, "STSDF"))
		x@data = x@data[sel, k, drop = FALSE]

# TG: Tom Gottfried reported at
# https://stat.ethz.ch/pipermail/r-sig-geo/2011-March/011231.html

	x@index = x@index[sel,, drop=FALSE]
    # inserted drop=FALSE to handle (length(i)==1 && length(j)==1) # TG

# now simplify everything, and drop any S/T not refered to:
	u1 = unique(x@index[,1])
	u2 = unique(x@index[,2])
	x@sp = x@sp[u1,]
	x@time = x@time[u2,]
	x@endTime = x@endTime[u2]
	x@index[,1] <- match(x@index[,1], u1)
	x@index[,2] <- match(x@index[,2], u2)

	if (drop) {
		if (length(s) == 1) { # space index has only 1 item:
			if (length(t) == 1)
				x = x@data[1,1,drop=TRUE]
			else {
				ix = index(x@time[x@index[,2]])
				if (is(ix, "Date"))
					x = xts(x@data, ix)
				else
					x = xts(x@data, ix, tzone = attr(x@time, "tzone"))
                # added index to achieve 
				#   (nrow(x)==length(order.by)) in index() # TG
			}
		} else if (length(t) == 1) { # only one time item
			if (is(x, "STSDF"))
				x = addAttrToGeom(x@sp[x@index[,1],], x@data, match.ID = FALSE)
            	# added index to achieve matching SpatialPoints and data.frame # TG
			else
				x = x@sp[x@index[,1],]
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

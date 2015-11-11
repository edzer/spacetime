# GEOMETRY ONLY:
# STS -> STF
as.STF.STS = function(from) {
	STF(from@sp, from@time, from@endTime)
}
setAs("STS", "STF", as.STF.STS)

# STF -> STS
as.STS.STF = function(from) {
	n = length(from@sp)
	m = nrow(from@time)
	index = cbind(rep(1:n, m), rep(1:m, each=n))
	STS(from@sp, from@time, index, from@endTime)
}
setAs("STF", "STS", as.STS.STF)

# STS -> STI
as.STI.STS = function(from) {
	# replicate the sp and time columns; keeps time always ordered?
	STI(from@sp[from@index[,1],], 
			from@time[from@index[,2]], 
			from@endTime[from@index[,2]])
}
setAs("STS", "STI", as.STI.STS)

# STF -> STI
as.STI.STF = function(from) {
	as(as(from, "STS"), "STI")
}
setAs("STF", "STI", as.STI.STF)

# GEOMETRY+ATTRIBUTES, *DF:
# STSDF -> STFDF
as.STFDF.STSDF = function(from) {
	# fill the partial grid with NAs
	# mainly taken from as.SPixDF.SGDF in sp:
   	data = list()
   	n = length(from@sp) * nrow(from@time)
	index = length(from@sp) * (from@index[,2] - 1) + from@index[,1]
   	for (i in seq(along = from@data)) {
		v = vector(mode(from@data[[i]]), n)
      	if (is.factor(from@data[[i]]))
			v = factor(rep(NA, n), levels = levels(from@data[[i]]))
		else
			v[-index] = NA
		v[index] = from@data[[i]]
		data[[i]] = v
   	}
   	data = data.frame(data, stringsAsFactors = FALSE)
   	names(data) = names(from@data)
	STFDF(from@sp, from@time, data, from@endTime)
}
setAs("STSDF", "STFDF", as.STFDF.STSDF)

# STFDF -> STSDF
as.STSDF.STFDF = function(from) {
	# take out the NA cells and fill the index
	# NOTE: does not (yet) take out empty space/time entities 
	# -- should this be optional?
	n = length(from@sp)
	m = nrow(from@time)
	index = cbind(rep(1:n, m), rep(1:m, each=n))
	# copied from sp:
	sel = apply(sapply(from@data, is.na), 1, function(x) !all(x))
	index = index[sel,,drop=FALSE]
	STSDF(from@sp, from@time, from@data[sel,,drop=FALSE], index, from@endTime)
}
setAs("STFDF", "STSDF", as.STSDF.STFDF)

# STSDF -> STIDF
as.STIDF.STSDF = function(from) {
	# replicate the sp and time columns; keeps time always ordered?
	sp = from@sp[from@index[,1],]
	if (is(sp, "SpatialPoints"))
		row.names(sp) = make.unique(row.names(sp))
	STIDF(sp, from@time[from@index[,2]], 
			from@data,
			from@endTime[from@index[,2]])
}
setAs("STSDF", "STIDF", as.STIDF.STSDF)

# STFDF -> STIDF
as.STIDF.STFDF = function(from) {
	as(as(from, "STSDF"), "STIDF")
}
setAs("STFDF", "STIDF", as.STIDF.STFDF)

zerodist.sp = function(from) {
	if(!is(from, "SpatialPoints")) {
		z = zerodist(SpatialPoints(myCoordinates(from)))
		if (nrow(z) == 0)
			return(1:length(from))
		sel = apply(z, 1, function(x) identical(from[x[1]], from[x[2]]))
		z = z[sel,]
		# convert to unique IDs, as zerodist(, unique.ID=TRUE) would do:
		id = 1:length(from)
		id[z[,1]] = id[z[,2]]
		id
	} else
		zerodist(from, unique.ID = TRUE)
}

as.STSDF.STIDF = function(from) {
	# find replicates in sp and time, and fill index:
	n = nrow(from@data)
	index = matrix(as.integer(NA), n, 2)
	# space:
	z = zerodist.sp(from@sp)
	uz = unique(z)
	sp = from@sp[uz,] # here, different attributes at duplicate features get lost...
	index[,1] = match(z, uz)
	# time -- use the fact that xts objects are in time order:
	ix = index(from@time)
	time = unique(ix)
	# not that simple -- TODO: glue together & check endTime...
	ir = rle(as.numeric(ix))$lengths
	index[,2] = rep(1:length(ir), ir)
	# check:
	stopifnot(!any(is.na(index)))
	# glue together:
	STSDF(sp, time, from@data, index)
}
setAs("STIDF", "STSDF", as.STSDF.STIDF)

as.STFDF.STIDF = function(from) {
	as(as(from, "STSDF"), "STFDF")
}
setAs("STIDF", "STFDF", as.STFDF.STIDF)

setAs("STT", "STI", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		endTime = do.call(c, lapply(from@traj, function(x) x@endTime))
		o = order(time)
		new("STI", ST(sp[o,,drop=FALSE], time[o], endTime[o])) # reorder here
	}
)
setAs("STTDF", "STIDF", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		attr(time, "tzone") = attr(index(from@traj[[1]]), "tzone")
		endTime = do.call(c, lapply(from@traj, function(x) x@endTime))
		STIDF(sp, time, from@data, endTime) # reorders there
	}
)
setAs("STIDF", "STTDF", 
	function(from) {
		if (is.null(from$burst))
			traj = list(as(from, "STI"))
		else
			traj = lapply(split(from, from$burst), function(x) as(x, "STI"))
		STIbox = STI(SpatialPoints(t(bbox(from@sp)), from@sp@proj4string),
				range(index(from)))
		new("STTDF", new("STT", STIbox, traj = traj), data = from@data)
	}
)

as.STFDF.Spatial = function(from) {
	#from@data$time = index(from@time)
	df = as.data.frame(t(as(from[,,1], "xts")))
	ret = addAttrToGeom(geometry(from@sp), df, match.ID = FALSE)
	# data.frame names will now be mangled time-like strings, so
	attr(ret, "time") = index(from@time) # to make it somehow accessible...
	ret
}
setAs("STFDF", "Spatial", as.STFDF.Spatial)

as.STIDF.Spatial = function(from) {
	from@data$time = index(from@time)
	addAttrToGeom(geometry(from@sp), from@data, match.ID = FALSE)
}
setAs("STIDF", "Spatial", as.STIDF.Spatial)
setAs("STSDF", "Spatial", function(from) as(as(from, "STIDF"), "Spatial"))
as.STI.Spatial = function(from)
	addAttrToGeom(geometry(from@sp), data.frame(time = index(from@time)), match.ID = FALSE)
setAs("STI", "Spatial", as.STI.Spatial)

#setClass("ltraj", representation("list"))
setOldClass("ltraj")

setAs("ltraj", "STTDF", 
	function(from) {
		d = do.call(rbind, from)
		ns = sapply(from, nrow)
		burst = sapply(from, function(x) attr(x, "burst"))
		id = sapply(from, function(x) attr(x, "id"))
		d$burst = factor(rep(burst, ns))
		d$id = factor(rep(id, ns))
		toSTI = function(x) {
			time = x[["date"]]
			ret = STI(SpatialPoints(x[c("x", "y")]), time)
			attr(ret, "burst") = attr(x, "burst")
			attr(ret, "id") = attr(x, "id")
			ret
		}
		rt = range(d$date)
		sp = SpatialPoints(cbind(range(d$x), range(d$y)))
		coordnames(sp) = c("x", "y")
		STIbox = STI(sp, rt)
		STTDF(STT(lapply(from, toSTI), STIbox), data = d)
	}
)

setAs("STTDF", "ltraj", 
	function(from) {
		x = as(from, "STIDF")
		xy = coordinates(x@sp)
		da = index(x@time)
		if (!requireNamespace("adehabitatLT", quietly = TRUE))
			stop("package adehabitatLT required to coerce to/from ltraj")
		ret = adehabitatLT::as.ltraj(xy, da, id = x[["id"]], burst = x[["burst"]])
	}
)

#setClass("stpp", representation("matrix"))
setOldClass("stpp", representation("matrix"))

setAs("STI", "stpp",
	function(from) {
		mat = cbind(coordinates(from@sp), as.numeric(index(from@time)))
		colnames(mat) = c("x", "y", "t")
		oldClass(mat) = "stpp"
		mat
	}
)
setAs("stpp", "STI",
	function(from) {
		sp = SpatialPoints(from[,1:2])
		time = as.POSIXct(from[,3], origin = "1970-01-01", tz = "GMT")
		STI(sp, time)
	}
)

setAs("STT", "data.frame", 
	function(from)
		do.call(rbind, lapply(from@traj, function(x) as(x, "data.frame")))
)

setAs("STTDF", "data.frame", 
	function(from)
		cbind(as(geometry(from), "data.frame"), from@data)
)
setAs("STTDF", "SpatialLines",
	function(from) {
		nm = names(from@traj)
		l = lapply(from@traj, function(x) Line(coordinates(x)))
		l2 = lapply(1:length(l), function(x) Lines(l[x], nm[x]))
		SpatialLines(l2)
	}
)

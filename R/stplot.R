if (!isGeneric("stplot"))
	setGeneric("stplot", function(obj, ...)
		standardGeneric("stplot"))

stplot.STFDF = function(obj, names.attr = trimDates(obj), ...,
		as.table = TRUE, at = NULL, cuts = 15, scales = list(draw = FALSE),
		animate = 0, mode = "xy", scaleX = 0, 
		auto.key = list(space = key.space), main,
		key.space = "right", type = 'l', do.repeat = TRUE,
		range.expand = 0.001) {

	ind = sp.ID = NULL # keep R CMD check happy
    z = names(obj@data)[1]
	seq.expand = function(min, max, length.out, expand) {
		r = max - min
		max = max + r * expand
		min = min - r * expand
		seq(min, max, length.out = length.out)
	}
	if (missing(at) && !is.factor(obj[[z]]))
		at = seq.expand(min(obj[[z]], na.rm = TRUE), max(obj[[z]], na.rm = TRUE), 
			length.out = ifelse(length(cuts) == 1, cuts + 1, length(cuts)), range.expand)
	if (missing(main)) {
		if (ncol(obj@data) == 1)
			main = names(obj@data)
		else
			main = NULL
	}
	if (mode == "ts") { # multiple time series
		if (!is.null(scales$draw) && scales$draw == FALSE)
			scales$draw = TRUE
		if (length(names(obj@data)) > 1) # , stack, add | which.var
			xyplot(values ~ time | ind, stack(obj), groups = sp.ID, 
				type = type, auto.key = auto.key, as.table = as.table, 
				scales = scales, main = main, ...)
		else
			xyplot(as.formula(paste(z, "~", "time")), 
				as.data.frame(obj), groups = sp.ID, 
				type = type, auto.key = auto.key, as.table = as.table, 
				scales = scales, main = main, ...)
	} else if (mode == "tp") { # time series in multiple panels
    	if (ncol(obj@data) == 1)
			xyplot(as.formula(paste(z, "~ time | sp.ID")), 
				as.data.frame(obj), type = type, auto.key = auto.key, 
				as.table = as.table, main = main, ...)
		else {
			n = names(obj@data)
			df = as.data.frame(obj)
			st = stack(df, n) # values ~ ind
			st$time = df$time
			st$sp.ID = df$sp.ID
			xyplot(as.formula(paste("values ~ time | sp.ID")), 
				st, type = type, groups = ind,
				auto.key = auto.key, as.table = as.table, 
				main = main, ...)
		}
	} else if (mode == "xt") { # space-time cross section == Hovmoeller
		if (missing(scales))
			scales = list(draw = TRUE)
		else
			scales$draw = TRUE
		s = longlat.scales(obj@sp, scales = scales, 
			xlim = bbox(obj@sp)[1,], ylim = bbox(obj@sp)[2,])
		cn = coordnames(obj@sp)
		if (scaleX == 1) {
			scales["x"] = s["x"]
			f = as.formula(paste(z, "~", cn[1], "+ time"))
		} else if (scaleX == 2) {
			scales["x"] = s["y"]
			f = as.formula(paste(z, "~", cn[2], "+ time"))
		} else
			f = as.formula(paste(z, "~ sp.ID + time"))
		dots = list(...)
		dots$scales = scales
		dots$main = main
		dots$at = at
		dots = append(list(f, as.data.frame(obj), as.table = as.table), dots)
		if (!is.factor(obj[[z]])) {
			dots$cuts = cuts
			dots$at = at
		}
		do.call(levelplot, dots)
	} else { # multiple spplots: panel for each time step.
		if (mode != "xy")
			stop("unknown value for argument mode")
		df = data.frame(reshape(as.data.frame(obj)[c(z, "time", "sp.ID")], 
			timevar = "time", idvar = "sp.ID", direction = "wide"))[, -1, drop=FALSE]
		x = addAttrToGeom(geometry(obj@sp), df, match.ID = FALSE)
		## OR:
		## x = as(obj, "Spatial")
		## x@data = data.frame(x@data) # cripples column names
		scales = longlat.scales(obj@sp, scales = scales, 
			xlim = bbox(obj@sp)[1,], ylim = bbox(obj@sp)[2,])
		if (animate > 0) {
			names.attr = rep(names.attr, length = ncol(df))
			i = 0
			while (do.repeat || i < ncol(df)) {
				timeStep = (i %% ncol(df)) + 1
				args = list(x[,timeStep], main = names.attr[timeStep], 
					as.table = as.table, auto.key = auto.key, scales = scales, ...)
				if (!is.factor(obj[[z]])) {
					args$cuts = cuts
					args$at = at
				}
				print(do.call(spplot, args))
				Sys.sleep(animate)
				i = i + 1
			}
		} else {
			args = list(x, names.attr = names.attr, as.table = as.table, 
				auto.key = auto.key, scales = scales, main = main, ...)
			if (!is.factor(obj[[z]])) {
				args$cuts = cuts
				args$at = at
			}
			if (is(obj@sp, "SpatialPoints"))
				args$key.space = key.space
			do.call(spplot, args)
		}
	}
}

#stplot.STIDF = function(obj, names.attr = index(obj@time), ...)
#	stplot(as(obj, "STFDF"), names.attr = names.attr, ...)

panel.stpointsplot = function(x, y, col, sp.layout, ...) {
    sppanel(sp.layout, panel.number())
	panel.xyplot(x, y, col = col, ...)
}

stplot.STIDF = function(obj, ..., names.attr = NULL,
		as.table = TRUE, scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = 'p', number = 6, tcuts, sp.layout = NULL,
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]) 
{
	if (ncol(obj@data) > 1)
		warning("plotting only the first mark or attribute")
	tix = index(obj)
	if (missing(tcuts))
		tcuts = seq(min(tix), max(tix), length.out = number + 1)
	else
		number = length(tcuts) - 1
	timeclass = findInterval(tix, tcuts, rightmost.closed = TRUE) # EJP, Mon Aug 17 12:07:12 CEST 2015
	data = obj@data[,1,drop=FALSE]
	if (number > 1) for (i in 2:number) {
		data = cbind(data, obj@data[,1])
		data[timeclass != i, i] = NA
		if (i == number)
			data[timeclass != 1, 1] = NA # deal with first time class
	}
	names(data) = make.names(names(data), TRUE)
	d = addAttrToGeom(obj@sp, data, FALSE)
	if (is.null(names.attr))
		names.attr = trimDates(tcuts[1:number])
	spplot(d, 1:number, names.attr = names.attr, as.table = as.table, 
		scales = scales, xlab = xlab, ylab = ylab, sp.layout = sp.layout,
		xlim = xlim, ylim = ylim, ...)
}

stplot.STI = function(obj, names.attr = NULL, ..., 
		as.table = TRUE,
		scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = 'p', number = 6, overlap = 0, asp,
		col = 1, panel = panel.stpointsplot, sp.layout = NULL,
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]
		) {
	f =  paste(rev(coordnames(obj@sp)), collapse=" ~ ")
	# further control time here?:
	f = paste(f, "| time")
	if (missing(asp))
		asp = mapasp(obj@sp)
	scales = longlat.scales(obj@sp, scales = scales, xlim, ylim)
	obj = as.data.frame(obj)
	if (is.numeric(number) && number > 1)
		obj$time = equal.count(obj$time, number = number, overlap = overlap)
	xyplot(as.formula(f), obj, asp = asp, type = type,
		as.table = as.table, scales = scales, xlab = xlab, ylab = ylab, 
		panel = panel, sp.layout = sp.layout, ...)
}

panel.sttrajplot = function(x, y, col, sp.layout, ..., GRP, lwd, lty = 1) {
    sppanel(sp.layout, panel.number())
	if (length(GRP) == 1 && length(lwd) == 1 && length(col) == 1)
		llines(x, y, lwd = lwd, col = col)
	else {
		ug = unique(GRP)
		lug = length(ug)
		lwd = rep(lwd, length.out = lug)
		lty = rep(lty, length.out = lug)
		col = rep(col, length.out = lug)

		for (i in ug) {
			sel = (GRP == i)
			llines(x[sel], y[sel], lwd = lwd[i], col = col[i])
		}
	}
}

stplot.STTDF = function(obj, names.attr = NULL, ..., 
		as.table = TRUE, by = c("none", "burst", "id", "time"), 
		scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = 'l', number = 6, overlap = 0, asp,
		col = 1, lwd = 1, lty = 1, panel = panel.sttrajplot, sp.layout = NULL,
		xlim = bbox(obj)[1,], ylim = bbox(obj)[2,]
		) {
	if (missing(asp))
		asp = mapasp(obj@sp)
	scales = longlat.scales(obj@sp, scales = scales, xlim, ylim)
	GRP = rep(1:length(obj@traj), times = sapply(obj@traj, length))

	f =  paste(rev(coordnames(obj@sp)), collapse=" ~ ")
	by = by[1]
	if (by != "none")
		f = paste(f, "|", by)
	obj = as(obj, "data.frame")
	if (is.numeric(number) && number > 1)
		obj$time = equal.count(obj$time, number = number, overlap = overlap)
	xyplot(as.formula(f), obj, asp = asp, type = type,
		as.table = as.table, scales = scales, xlab = xlab, ylab = ylab, 
		panel = panel, sp.layout = sp.layout, col = col, lwd = lwd, 
		lty = lty, ..., GRP = GRP)
}

setMethod("stplot", signature("STTDF"), stplot.STTDF)
setMethod("stplot", signature("STT"), stplot.STTDF)
setMethod("stplot", signature("STFDF"),  stplot.STFDF)
setMethod("stplot", signature("STSDF"),
          function(obj, ...) stplot.STIDF(as(obj, "STIDF"), ...))
setMethod("stplot", signature("STIDF"), stplot.STIDF)
setMethod("stplot", signature("STI"), stplot.STI)

stackST = function(x, select, ...) {
	nc = ncol(x@data)
	df = stack(x@data)
	g = as.data.frame(geometry(x))
	gf = do.call(rbind, lapply(1:nc, function(x) g))
	data.frame(gf, df)
}
stack.STFDF = stackST
stack.STSDF = stackST
stack.STIDF = stackST

trimDates = function(x) {
	if (is(x, "ST"))
		x = index(x@time)
	it = as.character(x)
	if (identical(grep("-01$", it), 1:length(it))) # all: day
		it = sub("-01$", "", it)
	if (identical(grep("-01$", it), 1:length(it))) # all: month
		it = sub("-01$", "", it)
	it
}

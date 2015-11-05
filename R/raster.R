# provided by (c) Robert J. Hijmans:
setAs('RasterStackBrick', 'STFDF',
    function(from) {
		# require(raster)
		if (!requireNamespace("raster", quietly = TRUE))
			stop("package raster required to coerce to/from raster")
		# TIME:
        tvals <- raster::getZ(from)
        if (is.null(tvals) || !timeBased(tvals))
            stop('Raster object must have Z values representing time')
		end.time = attr(from, "end.time")
		if (is.null(end.time))
			endTime = as.POSIXct(tvals)
		else
			endTime = as.POSIXct(end.time)
		# SPACE:
        # double coercion to avoid sparse SpatialPixels
        # but should be efficient
        sx <- as(as(from, 'SpatialGrid'), 'SpatialPixels')
		# ATTRIBUTES:
        d <- as.data.frame(as.vector(raster::getValues(from)))
        #names(d) = paste(names(from), collapse=".") # could do better?
		names(d) = names(from)[1]
        STFDF(sx, time = tvals, endTime = endTime, data = d)
    }
)

setAs('STFDF', 'RasterBrick',
    function(from) {
		if (!requireNamespace("raster", quietly = TRUE))
			stop("package raster required to coerce to/from raster")
        time <- from@time
        nc <- dim(from)[3]
		lt <- dim(from)[2]
        r <- raster::raster(from@sp)
        b <- raster::brick(r, nl = lt * nc)
        b <- raster::setZ(b, rep(time, nc)) # rep changes some time formats
        names(b) <- paste0(rep(colnames(from@data), each=lt), as.character(time))
		for (i in 1:nc) {
			for (j in 1:lt) {
				l = from[,j,i]
				fullgrid(l) = TRUE
				b = raster::setValues(b, as.numeric(l[[1]]), j + (i - 1) * lt)
			}
		}
		b
    }
)

setMethod('stplot', 'RasterStackBrick', 
    function(obj, ..., maxpixels = 50000) {
		if (!requireNamespace("raster", quietly = TRUE))
			stop("package raster required to coerce to/from raster")
		# require(raster)
        ob <- raster::sampleRegular(obj, 
			size = maxpixels, asRaster = TRUE, useGDAL = TRUE)
        ob <- as(ob, 'STFDF')
        stplot(ob, ...)
    }
)

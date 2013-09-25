# provided by (c) Robert J. Hijmans:
setAs('RasterStackBrick', 'STFDF',
    function(from) {
		require(raster)
		# TIME:
        tvals <- getZ(from)
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
        d <- as.data.frame(as.vector(getValues(from)))
        names(d) = paste(names(from), collapse=".") # could do better?
        STFDF(sx, time = tvals, endTime = endTime, data = d)
    }
)

setMethod('stplot', 'RasterStackBrick', 
    function(obj, ..., maxpixels=50000) {
		require(raster)
        ob <- sampleRegular(obj, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
        ob <- as(ob, 'STFDF')
        stplot(ob, ...)
    }
)

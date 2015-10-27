read.tgrass = function(fname, localName = TRUE, useTempDir = TRUE, isGeoTiff = TRUE) {
	stopifnot(isGeoTiff) # else: do sth for features.
	# require(raster)
	dir = ifelse(useTempDir, tempdir(), getwd())
	if (localName)
		fname = file.path(getwd(), fname)
	stopifnot(file.exists(fname))
	untar(fname, exdir = dir)
	# start reading:
	tab = read.table(file.path(dir, "list.txt"), sep = "|", as.is=TRUE)
	crs = paste(read.table(file.path(dir, "proj.txt"), as.is=TRUE)[[1]], 
		collapse=" ")
	if (requireNamespace("raster", quietly = TRUE)) {
		r = raster::stack(file.path(dir, paste(tab[[1]], ".tif", sep="")))
		proj4string(r) = CRS(crs)
		# get start time:
		start.time = as.POSIXct(tab[[2]])
		r = raster::setZ(r, start.time, name = 'time')
		# end time:
		attr(r, "end.time") = as.POSIXct(tab[[3]]) # will be used in coercion to STFDF
		r
	} else
		stop("package raster required for read.tgras")
}

write.tgrass = function(obj, fname, ...) {
	# require(raster)
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required for write.tgrass")
	if (is(obj, "STFDF")) {
		end.time = obj@endTime
		#obj = as(obj, "RasterStack")
		obj = as(obj, "RasterBrick")
	} else
		end.time = NULL
	n = paste(names(obj), ".tif", sep = "")
	olddir = getwd()
	dir = tempdir() # gets the same as read.tgrass got!
	setwd(dir)
	# write .tifs:
	for (i in 1:(raster::nlayers(obj)))
		raster::writeRaster(raster::raster(obj, layer=i), n[i], ...)
	# write proj.txt:
	write.table(data.frame(x = proj4string(obj)), "proj.txt", col.names=FALSE, 
		quote = FALSE, row.names = FALSE)
	# write list.txt:
	start.time = raster::getZ(obj)
	if (is.null(end.time))
		end.time = start.time
	tab = data.frame(names(obj), start.time, end.time)
	write.table(tab, "list.txt", sep = "|", col.names = FALSE, 
		quote = FALSE, row.names = FALSE)
	# write init.txt:
	f = file("init.txt", "w")
	cat("stds_type=strds\n", file = f)
	cat("format=GTiff\n", file = f)
	cat("temporal_type=absolute\n", file = f)
	cat("semantic_type=mean\n", file = f)
	cat("number_of_maps=", raster::nlayers(obj), "\n", file = f, sep="")
	e = raster::extent(obj)
	cat("north=", e@ymax, "\n", file = f, sep="")
	cat("south=", e@ymin, "\n", file = f, sep="")
	cat("east=", e@xmax, "\n", file = f, sep="")
	cat("west=", e@xmin, "\n", file = f, sep="")
	close(f)
	# tar.gz the thing:
	#tar(file.path(olddir, fname), compression = "gzip")
	cmd = paste("tar zcf", file.path(olddir, fname), ".")
	system(cmd)
	setwd(olddir)
}

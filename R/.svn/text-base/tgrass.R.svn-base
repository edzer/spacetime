read.tgrass = function(fname, localName = TRUE, useTempDir = TRUE, isGeoTiff = TRUE) {
	stopifnot(isGeoTiff) # else: do sth for features.
	require(raster)
	dir = ifelse(useTempDir, tempdir(), getwd())
	if (localName)
		fname = file.path(getwd(), fname)
	stopifnot(file.exists(fname))
	untar(fname, exdir = dir)
	# start reading:
	tab = read.table(file.path(dir, "list.txt"), sep = "|", as.is=TRUE)
	crs = paste(read.table(file.path(dir, "proj.txt"), as.is=TRUE)[[1]], collapse=" ")
	r = raster::stack(file.path(dir, paste(tab[[1]], ".tif", sep="")))
	proj4string(r) = CRS(crs)
	# get start time:
	start.time = as.POSIXct(tab[[2]])
	r = setZ(r, start.time, name = 'time')
	# end time:
	attr(r, "end.time") = as.POSIXct(tab[[3]]) # will be used in coercion to STFDF
	r
}

write.tgrass = function(obj, fname) {
	require(raster)
	if (is(obj, "STFDF")) {
		end.time = obj@endTime
		obj = as(obj, "RasterStack")
	} else
		end.time = NULL
	n = paste(names(obj), ".tif", sep = "")
	olddir = getwd()
	dir = tempdir() # gets the same as read.tgrass got!
	setwd(dir)
	# write .tifs:
	for (i in 1:nlayers(obj))
		writeRaster(raster(obj, layer=i), n[i])
	# write proj.txt:
	write.table(data.frame(x = proj4string(obj)), "proj.txt", col.names=FALSE, quote = FALSE, row.names = FALSE)
	# write list.txt:
	start.time = getZ(obj)
	if (is.null(end.time))
		end.time = start.time
	tab = data.frame(names(obj), start.time, end.time)
	write.table(tab, "list.txt", sep = "|", col.names = FALSE, quote = FALSE, row.names = FALSE)
	# tar.gz the thing:
	#tar(file.path(olddir, fname), compression = "gzip")
	cmd = paste("tar zcf", file.path(olddir, fname), ".")
	system(cmd)
	setwd(olddir)
}

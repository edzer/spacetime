library(spacetime)
if (require(raster, quietly = TRUE)) {
 x <- brick(system.file("external/rlogo.grd", package="raster"))
 x <- setZ(x, as.Date(1:nlayers(x), origin = "1970-01-01"))
 stplot(x)
}

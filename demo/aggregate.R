library(sp)
library(spacetime)

library(maptools)
nc <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
	proj4string=CRS("+proj=longlat +datum=NAD27 +ellps=clrk66"))

n = 1000
pts = spsample(nc, n, "random")

# we now create a time stamp that depends on county:
i = over(pts, geometry(nc)) # gets county index
dt = as.Date("1981-02-03") + i
sti = STIDF(pts, dt, data.frame(i = rep(1,n), d = dt, z = rnorm(n)))

# aggregate to [states x date] ST blocks:
stf = STF(nc, unique(sort(dt)))
pts.nc = aggregate(sti[,,"z"], stf, mean, na.rm = TRUE)

# compare state id's:
nc$id = 1:100
plot(nc)
text(coordinates(nc), labels = nc$id, col='red')

# to average z values:
stplot(pts.nc[,1:20])

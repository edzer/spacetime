###################################################
### chunk number 19: 
###################################################
library(sp)
library(spacetime)
if (require(gstat)) {
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"


###################################################
### chunk number 20: 
###################################################
library(mapdata)
plot(wind.loc, xlim = c(-11,-5.4), ylim = c(51,55.5), axes=T, col="red")
map("worldHires", add=T, col = grey(.5))
text(coordinates(wind.loc), pos=1, label=wind.loc$Station, cex=.7)


###################################################
### chunk number 21: 
###################################################
wind[1:3,]
wind.loc[1:3,]


###################################################
### chunk number 22: 
###################################################
wind$time = ISOdate(wind$year+1900, wind$month, wind$day)
wind$jday = as.numeric(format(wind$time, '%j'))


###################################################
### chunk number 23: 
###################################################
# match order of columns in wind to Code in wind.loc;
# convert to utm zone 29, to be able to do interpolation in
# proper Euclidian (projected) space:
pts = coordinates(wind.loc[match(names(wind[4:15]), wind.loc$Code),])
pts = SpatialPoints(pts)
proj4string(pts) = "+proj=longlat +datum=WGS84"
if (require(rgdal)) {
pts = spTransform(pts, CRS("+proj=utm +zone=29 +datum=WGS84"))
stations = 4:15
# note the t() in:
w = STFDF(pts, wind$time, data.frame(values = as.vector(t(wind[stations]))))

library(maptools)
m = map2SpatialLines(
	map("worldHires", xlim = c(-11,-5.4), ylim = c(51,55.5), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
m = spTransform(m, CRS("+proj=utm +zone=29 +datum=WGS84"))

# setup grid
grd = SpatialPixels(SpatialPoints(makegrid(m, n = 300)),
	proj4string = proj4string(m))
# grd$t = rep(1, nrow(grd))
#coordinates(grd) = ~x1+x2
#gridded(grd)=TRUE

# select april 1961:
w = w[, "1961-04"]

covfn = function(x,y) { 
	du = spDists(coordinates(x), coordinates(y))
	t1 = as.numeric(index(x)) # time in seconds
	t2 = as.numeric(index(y)) # time in seconds
	dt = abs(outer(t1, t2, "-"))
	# separable, product covariance model:
	0.6 * exp(-du/750000) * exp(-dt / (1.5 * 3600 * 24))
}

n = 10
tgrd = seq(min(index(w)), max(index(w)), length=n)
if (FALSE) {
pred = krige0(sqrt(values)~1, w, STF(grd, tgrd), covfn)
wind.pr = STFDF(grd, tgrd, data.frame(pred = pred))


###################################################
### chunk number 24: 
###################################################
spl = list(list("sp.points", pts, first=F, cex=.5),
	list("sp.lines", m, col='grey'))
stplot(wind.pr, col.regions=bpy.colors(),
	par.strip.text = list(cex=.5), sp.layout = spl)
summary(wind.pr)
}
}
}

require(sp)
require(spacetime)
options(xts_check_TZ=FALSE)

# go through time matching first:
x = as.POSIXct("2000-01-01", tz="GMT") + (0:9) * 3600
y = x + 1
y[1] = y[2]
format(x, tz="GMT")
format(y, tz="GMT")
end.x = delta(x)
end.y = delta(y)
timeMatch(y, y)
timeMatch(y, y, end.x = end.y, end.y = end.y)

timeMatch(x, y)
timeMatch(x, y, end.y = end.y)
timeMatch(x, y, end.x = end.x)
timeMatch(x, y, end.x = end.x, end.y = end.y)

timeMatch(x, y, returnList = TRUE)
timeMatch(x, y, returnList = TRUE, end.y = end.y)
timeMatch(x, y, returnList = TRUE, end.x = end.x)
timeMatch(x, y, returnList = TRUE, end.x = end.x, end.y = end.y)

end.x = delta(x)
y = x + 1 # don't replicate the first
end.y = delta(y)

a = timeMatch(x, y, end.x = end.x, end.y = end.y)
library(intervals)
b = timeMatch(x, y, end.x = end.x, end.y = end.y)
detach()
all.equal(a, b)

a = timeMatch(x, y, end.y = end.y)
library(intervals)
b = timeMatch(x, y, end.y = end.y)
detach()
all.equal(a, b)

a = timeMatch(x, y, end.x = end.x)
library(intervals)
b = timeMatch(x, y, end.x = end.x)
detach()
all.equal(a, b)

# with end points:

# next, try ST?DF objects:
t = as.POSIXct("2010-05-01", tz="GMT")+3600*1:10
xy = SpatialPixels(SpatialPoints(expand.grid(1:10,1:10)))
y = STFDF(xy, t, data.frame(a = 1:1000))
stplot(y)
x = y
all(over(x, y) == 1:1000)

t0 = c(t[1]-1,t[c(2,4,10)]) # c() drops TZ?
attr(t0, "tzone") = attr(t, "tzone")
x = c(4,5,6,8)
xy = SpatialPixels(SpatialPoints(expand.grid(x,rep(1,4))))
x = STFDF(xy, t0, data.frame(a = 1:64))
over(x, y)

x = as(x, "STSDF")
over(x, y)

xy = SpatialPoints(cbind(c(1,3,5,10),c(5, 3, 8, 2)))
x = STIDF(xy, t0, data.frame(a = 1:4))
over(x,y)

t1 = c(t[1]-1,t[c(2,2,10)])
attr(t1, "tzone") = attr(t, "tzone")
x1 = STIDF(xy[c(1,2,2,4)], t1, data.frame(a = 1:4))

over(x1, x)
over(x, x1)
over(x1, x, returnList = TRUE)
over(x, x1, returnList = TRUE)

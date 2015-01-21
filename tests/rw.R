library(sp)
library(spacetime)

m = 3# nr of trajectories
n = 100 # length of each
l = vector("list", m)
t0 = as.POSIXct("2013-05-05",tz="GMT")
set.seed(1331)
for (i in 1:m) {
    x = cumsum(rnorm(n))
    y = cumsum(rnorm(n))
    sp = SpatialPoints(cbind(x,y))
    #t = t0 + (0:(n-1) + (i-1)*n) * 60
    t = t0 + (0:(n-1) + (i-1)*n/2) * 60
    l[[i]] = STI(sp, t)
}
stt= STT(l)
sttdf = STTDF(stt, data.frame(attr = rnorm(n*m), id = paste("ID", rep(1:m, each=n))))
x = as(stt, "STI")
stplot(sttdf, col=1:m, scales=list(draw=TRUE))
stplot(sttdf, by = "id")
stplot(sttdf[1])
stplot(sttdf[1])

p = Polygon(cbind(x=c(-20,-15,-15,-20,-20),y=c(10,10,15,15,10)))
pol=SpatialPolygons(list(Polygons(list(p), "ID")))
if (require(rgeos)) {
  stplot(sttdf[pol])
  names(sttdf[pol]@traj)
  stplot(sttdf[1:2],col=1:2)
  stplot(sttdf[,t0])
  stplot(sttdf[,"2013"])
  stplot(sttdf[pol,"2013"])
  is.null(sttdf[pol,t0])
}

library(sp)
library(spacetime)

## ------------------------------------------------------------------------
IDS = c("079", "095", "111", "127", "143", "159", "175")

## ------------------------------------------------------------------------
# Geolife trajectories,
# documentation: http://research.microsoft.com/apps/pubs/?id=152176 :
# data: http://research.microsoft.com/en-us/projects/geolife/
# or http://ftp.research.microsoft.com/downloads/b16d359d-d164-469e-9fd4-daa38f2b2e13/Geolife%20Trajectories%201.2.zip
setwd("/home/edzer/Downloads/Geolife Trajectories 1.3/Data/")
i = j = 1
crs = CRS("+proj=longlat +datum=WGS84")
pb = txtProgressBar(style = 3, max = length(IDS))
elev = numeric(0)
lst0 = list()
for (d in IDS) {
	dir = paste(d, "Trajectory", sep = "/")
	lst = list()
	files = list.files(dir, pattern = "*plt", full.names = TRUE)
	i = 1
	for (f in files) {
		tab = read.csv(f, skip = 6, stringsAsFactors=FALSE)
		tab$time = as.POSIXct(paste(tab[,6],tab[,7]))
		tab[tab[,4] == -777, 4] = NA # altitude 
		tab = tab[,-c(3,5,6,7)]
		names(tab) = c("lat", "long", "elev", "time")
		if (all(tab$lat > -90 & tab$lat < 90 & tab$long < 360 
				& tab$long > -180)) {
			stidf = STIDF(SpatialPoints(tab[,2:1], crs), tab$time, tab)
			conn = tab[-1,"elev",drop=FALSE]
			lst[[i]] = CreateTrack(stidf, conn)
			i = i+1
		}
	}
	names(lst) = files
	lst0[[j]] = CreateTracks(lst)
	setTxtProgressBar(pb, j)
	j = j+1
}
names(lst0) = IDS
df = data.frame(IDS=IDS)
TR = TracksCollection(tracksCollection = lst0, tracksCollectionData = df)
object.size(TR)

plot(TR, xlim=c(116.3,116.5),ylim=c(39.8,40))
stplot(TR, xlim=c(116.3,116.5),ylim=c(39.8,40), col = 1:20)

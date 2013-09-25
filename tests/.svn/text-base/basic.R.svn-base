options(digits=5)
options(xts_check_TZ=FALSE)
###################################################
### chunk number 4: 
###################################################
library(sp)
library(spacetime)
set.seed(13579) # to make outcome a bit predictable!
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = as.POSIXct("2010-08-05", tz="GMT")+3600*(10:13)
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID", 1:length(mydata), sep = "_")
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)


###################################################
### chunk number 5: 
###################################################
format(as.data.frame(stfdf, row.names = IDs), tz="GMT")
format(as(stfdf, "data.frame")[1:4,], tz="GMT")

###################################################
### chunk number 6: 
###################################################
unstack(stfdf)
t(unstack(stfdf))
unstack(stfdf, which = 2)

###################################################
### chunk number 7: 
###################################################
as(stfdf[,,1], "xts")
as(stfdf[,,2], "xts")

###################################################
### chunk number 8: 
###################################################
stfdf[[1]]
stfdf[["values"]]
stfdf[["newVal"]] <- rnorm(12)
stfdf$ID
stfdf$ID = paste("OldIDs", 1:12, sep="")
stfdf$NewID = paste("NewIDs", 12:1, sep="")
stfdf


###################################################
### chunk number 9: 
###################################################
stfdf[,1] # SpatialPointsDataFrame:
stfdf[,,1]
stfdf[1,,1] # xts
stfdf[,,"ID"]
stfdf[1,,"values", drop=FALSE] # stays STFDF:
stfdf[,1, drop=FALSE] #stays STFDF

###################################################
### chunk number 10: 
###################################################
showClass("STSDF")

###################################################
### chunk number 11: 
###################################################
showClass("STIDF")
sp = expand.grid(x = 1:3, y = 1:3)
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = as.POSIXct("2010-08-05", tz="GMT")+3600*(11:19)
m = 1:9 * 10 # means for each of the 9 point locations
mydata = rnorm(length(sp), mean=m)
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3),ID=IDs)
stidf = STIDF(sp, time, mydata)
stidf


###################################################
### chunk number 12: 
###################################################
stidf[1:2,]


###################################################
### chunk number 13: 
###################################################
stfdf[,time[3]]


###################################################
### chunk number 14: 
###################################################
class(stfdf[,time[3],drop=FALSE])

###################################################
### chunk number 15: 
###################################################
stfdf[1, , "values"]

###################################################
### chunk number 16: 
###################################################
class(stfdf[1,drop=FALSE])


###################################################
### chunk number 17: 
###################################################
class(stfdf)
class(as(stfdf, "STSDF"))
class(as(as(stfdf, "STSDF"), "STIDF"))
class(as(stfdf, "STIDF"))


###################################################
### chunk number 18: 
###################################################
x = as(stfdf, "STIDF")
class(as(x, "STSDF"))
class(as(as(x, "STSDF"), "STFDF"))
class(as(x, "STFDF"))
xx = as(x, "STFDF")
identical(stfdf, xx)

stsdf = as(stfdf, "STSDF")
stsdf[[1]]
stsdf[["values"]]
stsdf[["newVal"]] <- rnorm(12)
stsdf$ID
stsdf$ID = paste("OldIDs", 1:12, sep="")
stsdf$NewID = paste("NewIDs", 12:1, sep="")
stsdf
stsdf[,1] # SpatialPointsDataFrame:
stsdf[,,1]
stsdf[1,,1] # xts
stsdf[,,"ID"]
stsdf[1,,"values", drop=FALSE] # stays STIDF:
stsdf[,1, drop=FALSE] #stays STIDF
format(as.data.frame(stsdf), tz="GMT")
format(as(stsdf, "data.frame"), tz="GMT")

stidf = as(stfdf, "STIDF")
stidf[[1]]
stidf[["values"]]
stidf[["newVal"]] <- rnorm(12)
stidf$ID
stidf$ID = paste("OldIDs", 1:12, sep="")
stidf$NewID = paste("NewIDs", 12:1, sep="")
stidf
stidf[,1] # SpatialPointsDataFrame:
stidf[,,1]
stidf[1,,1] # xts
stidf[,,"ID"]
stidf[1,,"values", drop=FALSE] # stays STIDF:
stidf[,1, drop=FALSE] #stays STIDF
format(as.data.frame(stidf), tz="GMT")
format(as(stidf, "data.frame"), tz="GMT")

sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
library(xts)
time = xts(1:4, as.POSIXct("2010-08-05", tz="GMT")+3600*(10:13))
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID", 1:length(mydata), sep = "_")
mydata = data.frame(values = signif(mydata,3), ID=IDs)
spx = SpatialPointsDataFrame(sp, data.frame(values = c("a", "b", "c")))
try(stfdf <- STFDF(spx, time, mydata))
timex = time
names(timex) = "values"
try(stfdf <- STFDF(sp, timex, mydata))
mydatax = mydata
names(mydatax) = "x"
try(stfdf <- STFDF(spx, timex, mydatax))
stfdf = STFDF(sp, timex, mydatax)
try(stfdf[["values"]] <- 1:12)
stfdf = STFDF(spx, time, mydatax)
try(stfdf[["values"]] <- 1:12)

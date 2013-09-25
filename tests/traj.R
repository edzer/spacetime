library(spacetime)
options(xts_check_TZ=FALSE)

###################################################
### code chunk number 51: spacetime.Rnw:1089-1098
###################################################
library("adehabitatLT")
data("puechabonsp")
locs = puechabonsp$relocs
xy = coordinates(locs)
da = as.character(locs$Date)
da = as.POSIXct(strptime(as.character(locs$Date),"%y%m%d", tz = "GMT"))
ltr = as.ltraj(xy, da, id = locs$Name)
foo = function(dt) dt > 100*3600*24
l2 = cutltraj(ltr, "foo(dt)", nextr = TRUE)

###################################################
### code chunk number 52: spacetime.Rnw:1102-1104 (eval = FALSE)
###################################################
sttdf = as(l2, "STTDF")
print(stplot(sttdf, by="time*id"))

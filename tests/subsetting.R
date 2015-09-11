# Sys.setenv(TZ="Europe/Berlin")
Sys.setenv(TZ="UTC")

library(sp)
library(spacetime)
data(air)
rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))

rr <- rural[,"2005-06"]

# conversion
rrSTFDF <- as(rr, "STFDF")
rrSTF <- as(rr, "STF")

rrSTSDF <- as(rr, "STSDF")
rrSTS <- as(rr, "STS")

rrSTIDF <- as(rr, "STIDF")
rrSTI <- as(rr, "STI")

# selection types
# STF..
rrSTFDF[1:2,3:4] # returns 4 values
rrSTFDF[1:2,4:3] # returns 4 values
rrSTFDF[cbind(2:1, 3:4)] # coerces matrix to vector, returns all time instances

rrSTF[2:1,] # returns 2 spatial and 2 temporal instances -> 4 instances
rrSTF[cbind(4:1,1:4)] # returns 4 spatial and all temporal instances

# STS..
rrSTSDF[1:2,3:4] # returns 4 values
rrSTSDF[cbind(1:2,3:4)] # returns 2 values for c(1,3) and c(2,4)
rrSTSDF[cbind(2:1,3:4)] # returns 2 values for c(1,3) and c(2,4)

rrSTS[1:2,3:4] # returns 2 spatial and 2 temporal instances and an index with 4 instances
rrSTS[cbind(1:2,3:4)] # returns 2 spatial and 2 temporal instances, but index with only 2 instances

# STI..
rrSTIDF[1:2,1:2] # returns two first entries
rrSTIDF[1:2,] # returns two first entries
rrSTIDF[,1:2] # returns two first entries
rrSTIDF[cbind(1:2,1:2)] # returns two first entries

rrSTI[1:2,1:2] # returns two first instances
rrSTI[1:2,] # returns two first instances
rrSTI[,1:2] # returns two first instances
rrSTI[cbind(1:2,1:2)] # returns two first instances

# ordered selection
###################
# is respected for STF..
rrSTFDF[1:2,1:2]@data
rrSTFDF[1:2,2:1]@data
rrSTFDF[2:1,1:2]@data

# and STS
rrSTSDF[1:2,1:2]@data
rrSTSDF[1:2,2:1]@data
rrSTSDF[2:1,1:2]@data

# and for STI in one time slice
rrSTIDF[1:2,1:2]@data
# rrSTIDF[2:1,2:1]@data

# 
rrSTIDF[c(1,2),c(1,2)]@data
# rrSTIDF[c(2,1),c(1,2)]@data
rrSTIDF[c(1,200),c(1,200)]@data
# rrSTIDF[c(200,1),c(1,200)]@data

# STx
rrSTF[1:2,1:2]@sp
rrSTF[2:1,1:2]@sp

rrSTS[1:2,1:2]
rrSTS[2:1,1:2]
rrSTS[2:1,2:1]

rrSTS[1:2,1:2]@sp
rrSTS[2:1,2:1]@sp

# multiple selection
####################
# returns multiple results for STF..
rrSTF[rep(2,3), 1:2]
rrSTFDF[rep(2,3), 1:2]
rrSTF[rep(2,3), rep(2,2)]
rrSTFDF[rep(2,3), rep(2,2)]

# returns multiple results for STS.. 
rrSTS[rep(2,3),1:2]
rrSTSDF[rep(2,3),1:2]@data
rrSTS[rep(2,3), rep(2,2)]
rrSTSDF[rep(2,3), rep(2,2)]@data

# returns unique results for STI..
rrSTI[rep(3,3),rep(3,3)]
rrSTI[cbind(5:1,1:5),]

rrSTIDF[rep(3,3),rep(3,3)]
rrSTIDF[cbind(5:1,1:5),]
rrSTIDF[cbind(1:5,1:5),]

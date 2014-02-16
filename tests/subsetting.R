library(sp)
library(spacetime)
data(air)
rr <- rural[,"2005-06"]

# matrix subsetting
M <- matrix(1:16,4,4)
M

# retrieve the top right sub-matrix 
M[1:2, 3:4]
# retrieve index pairs c(1,3) and c(2,4)
as.data.frame(M)[cbind(1:2,3:4)]

# what happens in sp?
data(meuse)
coordinates(meuse) ~ x+y

# multiple selection
meuse[rep(3,3), "zinc"]
# ordering is respected
meuse[1:3, "zinc"]
meuse[3:1, "zinc"]

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
rrSTFDF[cbind(1:2,3:4)] # coerces matrix to vector, returns all time instances

rrSTF[1:2,3:4] # returns 2 spatial and 2 temporal instances -> 4 instances
rrSTF[cbind(1:2,3:4)] # returns 4 spatial and all temporal instances

# STS..
rrSTSDF[1:2,3:4] # returns 4 values
rrSTSDF[cbind(1:2,3:4)] # returns 2 values for c(1,3) and c(2,4)

rrSTS[1:2,3:4] # returns 2 spatial and 2 temporal instances and an index with 4 instances
rrSTS[cbind(1:2,3:4)] # returns 2 spatial and 2 temporal instances, but index with only 2 instances

# STI..
rrSTIDF[1:2,3:4] # empty
rrSTIDF[1:2,1:2] # returns two first entries
rrSTIDF[1:2,] # returns two first entries
rrSTIDF[,1:2] # returns two first entries
rrSTIDF[cbind(1:2,1:2)] # returns two first entries twice

rrSTI[1:2,3:4] # empty
rrSTI[1:2,1:2] # returns two first instances
rrSTI[1:2,] # returns two first instances
rrSTI[,1:2] # returns two first instances
rrSTI[cbind(1:2,1:2)] # returns two first instances twice

# ordered selection
###################
# is respected for STF..
rrSTFDF[1:2,1:2]@data
rrSTFDF[2:1,1:2]@data

# but re-ordered for STS.. and STI..
rrSTSDF[1:2,1:2]@data
rrSTSDF[2:1,1:2]@data
rrSTSDF[1:2,2:1]@data

rrSTIDF[1:2,1:2]@data
rrSTIDF[2:1,2:1]@data

# STx
rrSTF[1:2,1:2]@sp
rrSTF[2:1,1:2]@sp

rrSTS[1:2,1:2]@sp
rrSTS[2:1,1:2]@sp

rrSTS[1:2,1:2]@sp
rrSTS[2:1,2:1]@sp

# multiple selection
####################
# returns multiple results for STF..
rrSTF[rep(2,3),1:2]
rrSTFDF[rep(2,3),1:2]@data

# returns unique results for STS.. and STI..
rrSTS[rep(2,3),1:2]
rrSTSDF[rep(2,3),1:2]@data

rrSTI[rep(2,3),rep(2,3)]
rrSTIDF[rep(2,3),rep(2,3)]@data

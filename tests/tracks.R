# Load required libraries.

library(sp)
library(spacetime)
library(rgdal)

# Create test objects. Do not change! Changes to the test objects are likely to
# have an impact on the test results. It is primarily validated against class
# and dimension. However, the test functions check for the first dimension only,
# since, in the majority of cases, a deviation of the second is not necessarily
# associated with a regression.

t0 = as.POSIXct(as.Date("2013-09-30", tz = "CET"))

# Person A, track 1.

x = c(7, 6, 5, 5, 4, 3, 3)
y = c(7, 7, 6, 5, 5, 6, 7)
n = length(x)
t = t0 + cumsum(runif(n) * 60)
crs = CRS("+proj=longlat")
stidf = STIDF(SpatialPoints(cbind(x, y), crs), t, data.frame(co2 = rnorm(n)))
A1 = Track(stidf)

# Person A, track 2.

x = c(7, 6, 6, 7, 7)
y = c(6, 5, 4, 4, 3)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x, y), crs), t, data.frame(co2 = rnorm(n)))
A2 = Track(stidf)

# Tracks for person A.

A = Tracks(list(A1 = A1, A2 = A2))

# Person B, track 1.

x = c(2, 2, 1, 1, 2, 3)
y = c(5, 4, 3, 2, 2, 3)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x, y), crs), t, data.frame(co2 = rnorm(n)))
B1 = Track(stidf)

# Person B, track 2.

x = c(3, 3, 4, 3, 3, 4)
y = c(5, 4, 3, 2, 1, 1)
n = length(x)
t = max(t) + cumsum(runif(n) * 60)
stidf = STIDF(SpatialPoints(cbind(x, y), crs), t, data.frame(co2 = rnorm(n)))
B2 = Track(stidf)

# Tracks for person B.

B = Tracks(list(B1 = B1, B2 = B2))

# Tracks collection.

Tr = TracksCollection(list(A = A, B = B))

all = list(A1, A2, B1, B2, A, B, Tr)

# Test methods.

checkClass = function(list, class) {
	stopifnot(all(sapply(list, function(x) class(x)[1] == class)))
}

checkDim = function(list, dim) {
	for(i in seq_along(list)) {
		element = list[[i]]
		if(class(element)[1] %in% c("data.frame", "xts", "STIDF", "SpatialPointsDataFrame"))
			stopifnot(dim(element)[1] == dim[i])
		else if(class(element)[1] == "Line")
			stopifnot(dim(element@coords)[1] == dim[i])
		else if(class(element)[1] == "Lines")
			# For simplification purposes, the number of Line elements (= number
			# of Tracks) is validated.
			stopifnot(length(element@Lines) == dim[i])
		else if(class(element)[1] %in% c("SpatialLines", "SpatialLinesDataFrame"))
			# For simplification purposes, the sum of the number of Line
			# elements (= total number of Tracks) is validated.
			stopifnot(sum(sapply(element@lines, function(x) length(x@Lines))) == dim[i])
		else
			warning(paste("Validation against dimension of class '", class(element)[1], "' is not yet supported.", sep = ""))	
	}
}

# Check coercion to segments.

res = lapply(all, function(x) as(x, "segments"))
checkClass(res, "data.frame")
dim = c(6, 4, 5, 5, 10, 10, 20)
checkDim(res, dim)

# Check coercion to data frame.

res = lapply(all, function(x) as(x, "data.frame"))
checkClass(res, "data.frame")
dim = c(7, 5, 6, 6, 14, 14, 28)
checkDim(res, dim)

# Check coercion to Line, Lines, SpatialLines and SpatialLinesDataFrame.

res = lapply(all[1:4], function(x) as(x, "Line"))
checkClass(res, "Line")
dim = c(7, 5, 6, 6)
checkDim(res, dim)

res = lapply(all[1:6], function(x) as(x, "Lines"))
checkClass(res, "Lines")
dim = c(1, 1, 1, 1, 2, 2)
checkDim(res, dim)

res = lapply(all, function(x) as(x, "SpatialLines"))
checkClass(res, "SpatialLines")
dim = c(1, 1, 1, 1, 2, 2, 4)
checkDim(res, dim)

res = lapply(all[5:length(all)], function(x) as(x, "SpatialLinesDataFrame"))
checkClass(res, "SpatialLinesDataFrame")
dim = c(2, 2, 4)
checkDim(res, dim)

# Check coercion to xts.

res = lapply(all, function(x) as(x, "xts"))
checkClass(res, "xts")
dim = c(7, 5, 6, 6, 12, 12, 24)
checkDim(res, dim)

# Check coercion to STIDF.

res = lapply(all, function(x) as(x, "STIDF"))
checkClass(res, "STIDF")
dim = c(7, 5, 6, 6, 12, 12, 24)
checkDim(res, dim)

# Check coercion to SpatialPointsDataFrame.

res = lapply(all, function(x) as(x, "SpatialPointsDataFrame"))
checkClass(res, "SpatialPointsDataFrame")
dim = c(7, 5, 6, 6, 12, 12, 24)
checkDim(res, dim)

# Check proj4string methods.

stopifnot(all(sapply(all, function(x) proj4string(x) == "+proj=longlat +ellps=WGS84")))

# Check coordnames methods.

stopifnot(all(sapply(all, function(x) coordnames(x) == c("x", "y"))))

# Check bbox methods.

lapply(all, function(x) bbox(x))

# Check selection methods.

stopifnot(class(Tr[1:2])[1] == "TracksCollection")
stopifnot(class(Tr[2])[1] == "Tracks")
stopifnot(class(Tr[2][1])[1] == "Track")
stopifnot(class(Tr[list(1:2, 2:3)])[1] == "TracksCollection")
stopifnot(class(Tr[list(integer(0), 2:3)])[1] == "Tracks")
stopifnot(class(Tr[list(integer(0), 2)])[1] == "Track")
stopifnot(class(Tr[list(1:2, 2:3), drop = FALSE])[1] == "TracksCollection")
stopifnot(class(Tr[list(integer(0), 2:3), drop = FALSE])[1] == "TracksCollection")
stopifnot(class(Tr[list(integer(0), 2), drop = FALSE])[1] == "TracksCollection")
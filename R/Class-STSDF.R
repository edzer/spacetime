setClass("STS", # space-time partial 
  contains = "ST", 
  slots = c(index = "matrix"),
  validity = function(object) {
    stopifnot(ncol(object@index) == 2)
	ix = unique(object@index[,1]) # space
	stopifnot(min(ix) >= 1 && max(ix) <= length(object@sp))
	ix = unique(object@index[,2]) # time
	stopifnot(min(ix) >= 1 && max(ix) <= nrow(object@time))
	return(TRUE)
  }
)

setClass("STSDF", # space-time partial data frame
  contains = "STS", 
  slots = c(data = "data.frame"),
  validity = function(object) {
	stopifnot(nrow(object@index) == nrow(object@data))
    #stopifnot(ncol(object@index) == 2)
	#ix = unique(object@index[,1]) # space
	#stopifnot(min(ix) >= 1 && max(ix) <= length(object@sp))
	#ix = unique(object@index[,2]) # time
	#stopifnot(min(ix) >= 1 && max(ix) <= nrow(object@time))
    .checkAttrIsUnique(object@sp, object@time, object@data)
	return(TRUE)
  }
)

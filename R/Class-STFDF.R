setClass("STF", # space-time full
  contains = "ST",
  validity = function(object) {
    return(TRUE)
  }
)

setClass("STFDF", # space-time full data frame
  contains = "STF", 
  slots = c(data = "data.frame"),
  validity = function(object) {
    stopifnot(nrow(object@data) == length(object@sp) * nrow(object@time))
    .checkAttrIsUnique(object@sp, object@time, object@data)
    return(TRUE)
  }
)

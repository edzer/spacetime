setClass("STT",  # space-time trajectory/ies without data values
  contains = "ST", 
  slots = c(traj = "list"),
  validity = function(object) {
    stopifnot(length(object@traj) > 0)
	stopifnot(length(object@sp) == 2)
	stopifnot(length(object@time) == 2)
	stopifnot(all(sapply(object@traj, class) == "STI"))
	stopifnot(!isTRUE(timeIsInterval(object)))
    return(TRUE)
  }
)

setClass("STTDF",  # space-time trajectory/ies with data values
  contains = "STT", 
  slots = c(data = "data.frame"),
  validity = function(object) {
	stopifnot(sum(sapply(object@traj, length)) == nrow(object@data))
    .checkAttrIsUnique(object@sp, object@time, object@data)
    return(TRUE)
  }
)

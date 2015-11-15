EOF = function(x, how = c("spatial", "temporal"), returnPredictions = TRUE, 
		...) { 
	.Deprecated("eof")
	sp = x@sp
	index = index(x@time)
	x = as(x, "xts") # matrix
	if (how[1] == "spatial") {
		pr = prcomp(~., data.frame(t(x)), ...)
		if (! returnPredictions)
			return(pr)
		x = addAttrToGeom(sp, data.frame(predict(pr)), TRUE)
	} else if (how[1] == "temporal") {
		pr = prcomp(~., data.frame(x), ...)
		if (! returnPredictions)
			return(pr)
		x = xts(data.frame(predict(pr)), index)
	} else
		stop("unknown mode: use spatial or temporal")
	names(x) = paste("EOF", 1:ncol(x), sep="")
	x
}

eof = function(x, how = c("spatial", "temporal"), returnEOFs = TRUE, ...) { 
	sp = x@sp
	index = index(x@time)
	x = as(x, "xts") # matrix
	switch(how[1], 
		"spatial" = {
			pr = prcomp(~., data.frame(x), ...)
			if (! returnEOFs)
				return(pr)
			x = addAttrToGeom(sp, data.frame(pr$rotation), FALSE)
		},
		"temporal" = {
			pr = prcomp(~., data.frame(t(x)), ...)
			if (! returnEOFs)
				return(pr)
			x = xts(data.frame(pr$rotation), index)
		},
		stop("unknown mode: use spatial or temporal")
	)
	names(x) = paste("EOF", 1:ncol(x), sep="")
	x
}

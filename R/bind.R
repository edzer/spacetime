cbind.ST = function (..., deparse.level = 1) {
	args = list(...)
	# cbind data slots:
	df = do.call(cbind, lapply(args, function(x) x@data))
	ret = args[[1]]
	ret@data = df
	ret
}

rbind.STIDF = function(..., deparse.level = 1) {
	args = list(...)
	df = do.call(rbind, lapply(args, function(x) x@data))
	time = do.call(c, lapply(args, function(x) index(x@time)))
	endTime = do.call(c, lapply(args, function(x) x@endTime))
	sp = do.call(rbind, lapply(args, function(x) x@sp))
	STIDF(sp, time, df, endTime)
}

rbind.STTDF = function(...) {
    dots = list(...)
    names(dots) <- NULL # bugfix Clement Calenge 100417
    df = do.call("rbind", lapply(dots, function(x) as(x, "STIDF")))
	as(df, "STTDF")
}

rbind.STFDF = function(..., deparse.level = 1) {
	args = list(...)
	n = names(args[[1]]@data)
	# as(do.call(rbind, lapply(args, function(x) as(x, "STIDF"))), "STFDF")
	sp = do.call(rbind, lapply(args, function(x) x@sp))
	# assuming that all STFDF have the same interval length
	endTime = unique(do.call(c, lapply(args, function(x) x@endTime)))
	args = lapply(args, function(x) as(x, "xts"))
	args = do.call(cbind, args)
	ret = stConstruct(args, sp, endTime = endTime)
	names(ret@data) = n
	ret
}

rbind.STSDF = function(..., deparse.level = 1) {
	# args = list(...)
	# as(do.call(rbind, lapply(args, function(x) as(x, "STIDF"))), "STSDF")
	args = list(...)
	n = names(args[[1]]@data)
	sp = do.call(rbind, lapply(args, function(x) x@sp))
	n.times <- sapply(args, function(x) length(index(x@time)))
	n.locs <- sapply(args, function(x) length(x@sp))
	  
	newTime <- unique(do.call(c, lapply(args, function(x) index(x@time))))
	newTime <- xts(1:length(newTime), newTime)
	
	newIndexTime <- unlist(lapply(args, function(x) match(index(x@time)[x@index[,2]], index(newTime))))
	newIndexSpace <- unlist(lapply(1:length(args), 
	                               function(i) rep(sum(c(0,n.locs)[1:i])+1:n.locs[i], n.times[i])))
	newIndex <- cbind(newIndexSpace, newIndexTime)
	colnames(newIndex) <- NULL
	# assuming that all STSDF have the same interval length
	endTime = unique(do.call(c, lapply(args, function(x) x@endTime)))
	
	df <- do.call(rbind, lapply(args, function(x) x@data))
	ret <- STSDF(sp, newTime, df,
	             newIndex, endTime)
	names(ret@data) = n
	ret
}
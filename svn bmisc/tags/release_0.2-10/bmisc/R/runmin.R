runmin <- function (x,...) UseMethod("runmin")

runmin.default <-function(x,window){
	require(zoo)
	ori <- x
	new <- rollmin(x,window,na.pad=T)
	new[is.na(new)] <- ori[is.na(new)]
	new <- smoothEnds(new,window)
	new
}


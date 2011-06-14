runmean <- function (x,...) UseMethod("runmean")

runmean.default <-function(x,window){

	ori <- x
	new <- rollmean(x,window,na.pad=T)
	new[is.na(new)] <- ori[is.na(new)]
	new <- smoothEnds(new,window)
	new
}


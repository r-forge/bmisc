runmax <- function (x,...) UseMethod("runmax")

runmax.default <- function(x,window){

	ori <- x
	new <- rollmax(x,window,na.pad=T)
	new[is.na(new)] <- ori[is.na(new)]
	new <- smoothEnds(new,window)
	new
}


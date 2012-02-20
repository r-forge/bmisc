cv <- function (x,...) UseMethod("cv")

cv.default <-
function(x, na.rm=T){
	sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
}

rsd <- function (x,...) UseMethod("rsd")

rsd.default <-
function(x, na.rm=T){
	abs(sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm))
}
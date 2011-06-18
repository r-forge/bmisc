cv <- function (x,...) UseMethod("cv")

cv.default <-
function(x, na.rm=T){
	sd(x,na.rm=na.rm)/abs(mean(x,na.rm=na.rm))
}

#
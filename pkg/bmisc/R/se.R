se <- function (x,...) UseMethod("se")

se.default <-function(x, na.rm=T){
	sd(x,na.rm=na.rm)/sqrt(n(x))
}


roundup <- function (x,...) UseMethod("roundup")

roundup.default <- function(x,numdigits=0){
	x <- x * 10^numdigits
	x <- ifelse(x<0,-trunc(abs(x)+0.5),trunc(x+0.5))
	x / 10^numdigits
}


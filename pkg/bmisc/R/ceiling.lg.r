ceiling.lg <- function (x) UseMethod("ceiling.lg")

ceiling.lg.default <- function(x){
	x <- roundup(x)
	y <- 10^(nchar(format(x,scientific = F))-1)
	ceiling(x / y) * y
}


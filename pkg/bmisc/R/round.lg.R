round.lg <- function (x) UseMethod("round.lg")

round.lg.default <- function(x){
	x <- roundup(x)
	y <- 10^(nchar(format(x,scientific = F))-1)
	roundup(x / y) * y
}



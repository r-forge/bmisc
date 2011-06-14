last <- function (x) UseMethod("last")

last.default <-function(x){
	x[length(x)]
}


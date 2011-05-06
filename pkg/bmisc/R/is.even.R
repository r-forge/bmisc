is.even <- function (x) UseMethod("is.even")

is.even.default <-function(x){
	(roundup(x) %% 2 == 0)
}


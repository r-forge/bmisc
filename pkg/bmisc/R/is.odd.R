is.odd <- function (x) UseMethod("is.odd")

is.odd.default <- function(x){
	(roundup(x) %% 2 != 0)
}


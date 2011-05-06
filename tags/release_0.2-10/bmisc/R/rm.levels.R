rm.levels <- function (factor) UseMethod("rm.levels")

rm.levels.default <- function(factor){
	as.factor(as.character(factor))
}


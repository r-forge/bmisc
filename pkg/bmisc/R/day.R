day <- function (x) UseMethod("day")

day.default <- function(x){
	as.numeric(format(x, "%j"))
}
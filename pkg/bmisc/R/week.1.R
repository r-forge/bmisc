week.1 <- function (x) UseMethod("week.1")

week.1.default <- function(x)
	{
		(day(x)-1) %/% 7 +1
	}
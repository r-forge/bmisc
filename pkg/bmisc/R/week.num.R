week.num <- function (x,...) UseMethod("week.num")

week.num.default <- function(x, day="sunday")
	{
	 if(day %nin% c("monday","sunday")) {stop("day must be 'monday' or 'sunday'")}
	 if(day=="sunday") {out <-as.numeric(format(x, "%U"))}
   if(day=="monday") {out <-as.numeric(format(x, "%W"))}
	 out	
	}
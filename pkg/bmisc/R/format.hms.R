format.hms <- function (sec) UseMethod("format.hms")

format.hms.default <- function(sec){
	minutes <- sec / 60
	hrs <- trunc(sec / 3600)
	out=paste(hrs,":",sprintf("%02.0f",trunc(minutes) - (60*hrs),2),":",sprintf("%02.0f",roundup((minutes - trunc(minutes)) * 60,2)),sep="")	
}


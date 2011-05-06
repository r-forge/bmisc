performance <- function (expr,...) UseMethod("performance")

performance.default <-function(expr, samples=1, gcFirst=TRUE){

	loc.frame <- parent.frame()
	results <- data.frame()
     
	for(i in 1:samples){
		if (gcFirst){ gc(FALSE) }
		expr <- substitute(expr)
    	time <- proc.time()
		eval(expr, envir = loc.frame)
    	new.time <- proc.time()
    	results <- rbind(results,as.vector(structure(new.time - time, class = "proc_time"))[1:3])
	}   
    
	test <<- results
	cat("\n\tAverage time per run:\n\t---------------------\n")
	cat("\tUser\t\tSystem\t\tElapsed\n")
	cat("\t",format(roundup(mean(results[1]),3),nsmall=3),"\t\t",format(roundup(mean(results[2]),3),nsmall=3),"\t\t",format(roundup(mean(results[3]),3),nsmall=3),sep="")
	
	cat("\n\n\tTotal time for all runs:\n\t------------------------\n")
	cat("\tUser\t\tSystem\t\tElapsed\n")
	cat("\t",format(roundup(sum(results[1]),3), nsmall=3),"\t\t",format(roundup(sum(results[2]),3),nsmall=3),"\t\t",format(roundup(sum(results[3]),3),nsmall=3),sep="")
	cat(" \n ")

}


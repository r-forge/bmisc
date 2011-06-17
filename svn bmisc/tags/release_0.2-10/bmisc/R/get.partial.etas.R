get.partial.etas <- function (model) UseMethod("get.partial.etas")

get.partial.etas.default <-function(model){
	require(gdata)
	n <- names(summary(model))
	cat("\nPartial eta squared values\n",sep="")
	cat("----------------------------\n\n",sep="")
	for(idx in 1:length(n)){
		m <- row.names(summary(model)[[idx]][[1]])
		ss <- summary(model)[[idx]][[1]]$"Sum Sq"
		if(length(ss)>1){
			cat(n[idx],"\n",sep="")	
			for(j in 1:(length(m)-1)){
				eta <- ifelse(length(ss) < 2, NA, ss[j] / (ss[j] + last(ss)))
				cat(m[j],": ",eta,"\n",sep="")
			}
			cat("---\n\n",sep="")
		}
	}
}


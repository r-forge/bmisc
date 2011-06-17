reject.z <- function (x,...) UseMethod("reject.z")

reject.z.default <-function(x,index=NULL,threshold=2) {
 if (is.null(index)) {
   index <- rep(1,length(x))
 }
 z <- rep(NA,length(x))
 
 .reject.z <- function(v,data,threshold) {
   
   d <- data[v]
   mean.d <- mean(d)
   sd.d <- sd(d)

   if (sd.d>0) {
     tmp.z <- (d - mean.d) / sd.d

     z[v] <<- d
     z[v][abs(tmp.z)>threshold] <<- NA
   }
   
   return(NULL)
 }
 tapply(1:length(x),index,.reject.z,x,threshold)

 z
}


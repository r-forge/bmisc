replace.z <- function (x,...) UseMethod("replace.z")

replace.z.default <-function(x,index=NULL,threshold=2) {
 if (is.null(index)) {
   index <- rep(1,length(x))
 }
 z <- rep(NA,length(x))
 
 .replace.z <- function(v,data,threshold) {
   
   d <- data[v]
   mean.d <- mean(d)
   sd.d <- sd(d)

   if (sd.d>0) {
     ma <- mean.d + (threshold * sd.d)
     mi <- mean.d - (threshold * sd.d)
     tmp.z <- (d - mean.d) / sd.d

     z[v] <<- d
     z[v][tmp.z>threshold] <<- ma
     z[v][tmp.z<(-1*threshold)] <<- mi
   }
   
   return(NULL)
 }
 tapply(1:length(x),index,.replace.z,x,threshold)
 z
}


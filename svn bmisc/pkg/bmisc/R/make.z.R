make.z <- function (x,...) UseMethod("make.z")

make.z.default <-function(x,index=NULL) {
 if (is.null(index)) {
   index <- rep(1,length(x))
 }
 z <- rep(NA,length(x))
 
 .make.z <- function(v,data) {
   d <- data[v]
   z[v][sd(d)>0] <<- (d - mean(d)) / sd(d)[sd(d)>0]
   return(NULL)
 }
 tapply(1:length(x),index,.make.z,x)
 z
}


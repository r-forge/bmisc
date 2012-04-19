make.z <- function (x,...) UseMethod("make.z")

make.z.default <-function(x) {

   z =  (x - mean(x,na.rm=T)) / sd(x,na.rm=T)

}


#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##   Calculates Orthogonal residuals                                           ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-07-29                                                            ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################
resid.ortho <- function (x,...) UseMethod("resid.ortho")
#plot.ortho <- function (x,...) UseMethod("plot.ortho")

resid.ortho.default <- function(slope, intercept, data)
{
        slope2=-1*(1/slope)
        intercept2=data[,2]-(slope2*data[,1])
        
        x.int=(intercept2-intercept)/(slope-slope2)
        y.int=slope*x.int+intercept
        val.orth=data.frame(x.int,y.int)
        
        
        x.int2=x.int[2:n(x.int)]
        x.int.diff=x.int2-x.int[1:(n(x.int)-1)]
        y.int2=y.int[2:n(y.int)]
        y.int.diff=y.int2-y.int[1:(n(y.int)-1)]
        dist1=c(0,y.int.diff/sin(atan(y.int.diff/x.int.diff)))
        dist1=dist1-mean(dist1, na.rm=TRUE)
        
        a1=data[,1]-x.int
        a2=data[,2]-y.int
        dist2=a1/sin(atan(a1/a2))
        res=cbind(data,x.int,y.int,y.res=dist2)
        res
}

resid.ortho.lmodel2= function(object,type="RMA"){
        sel=which(object$regression.results==type)
        
        intercept=object$regression.results[sel,][,2]
        slope=object$regression.results[sel,][,3]
        data=as.data.frame(object[2:1])
        
        resid.ortho.default(slope=slope,intercept=intercept,data=data)
}





#res=resid.ortho(value=data.frame(x,y))
#for(i in 1:length(x)){
#        lines(x=c(res$x[i],res$x.int[i]), y=c(res$y[i], res$y.int[i]))
#}

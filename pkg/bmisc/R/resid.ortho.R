#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##   Calculates Orthogonal residuals                                           ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-07-29															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################
resid.ortho <- function (x,...) UseMethod("resid.ortho")

resid.ortho.default <- function(b=5, c=2, value, type=type)
{
    b2=-1*(1/b)
    c2=value[,2]-(b2*value[,1])
    
    x.int=(c2-c)/(b-b2)
    y.int=b*x.int+c
    val.orth=data.frame(x.int,y.int)
    
    
    x.int2=x.int[2:n(x.int)]
    x.int.diff=x.int2-x.int[1:(n(x.int)-1)]
    y.int2=y.int[2:n(y.int)]
    y.int.diff=y.int2-y.int[1:(n(y.int)-1)]
    dist1=c(0,y.int.diff/sin(atan(y.int.diff/x.int.diff)))
    dist1=dist1-mean(dist1, na.rm=TRUE)
    
    a1=value[,1]-x.int
    a2=value[,2]-y.int
    dist2=a1/sin(atan(a1/a2))
    res=cbind(value,x.int,y.int,y.res=dist2)
    res
}

resid.ortho.lmodel2= function(object,type="RMA"){
    sel=which(object$regression.results==type)
   
    c=object$regression.results[sel,][,2]
    b=object$regression.results[sel,][,3]
    value=as.data.frame(object[2:1])
    
    resid.ortho.default(b=b,c=c,value=value)
}


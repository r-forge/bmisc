#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##  Based on dose.p() from MASS package                                        ##
##  Inverse prediction with SE estimate for generalized models:                ##
##          usefull for LD50 estimate                                          ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-07-28															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################


inv.pred=function (obj, cf = 1:2, p = 0.5) 
{
    eta <- family(obj)$linkfun(p)
    b <- coef(obj)[cf]
    x.p <- (eta - b[1L])/b[2L]
    names(x.p) <- NULL
    pd <- -cbind(1, x.p)/b[2L]
    SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
    res <- data.frame(prob=p, est=x.p, se= SE)
    
    res
}


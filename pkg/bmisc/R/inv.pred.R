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


inv.pred=function (object, cf = 1:2, y = 0.5)
{
    eta <- family(object)$linkfun(y)
    b <- coef(object)[cf]
    x.y <- (eta - b[1L])/b[2L]
    names(x.y) <- NULL
    yd <- -cbind(1, x.y)/b[2L]
    SE <- sqrt(((yd %*% vcov(object)[cf, cf]) * yd) %*% c(1, 1))
    res <- data.frame(y=y, est=x.y, se= SE)

    res
}

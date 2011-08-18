#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-08-17															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################

find.beta=function(beta=0.5, minv, maxv,prop=0.1){

        if(minv>maxv){
                warning(paste("Be sure that 'minv' is a smaller value than 'maxv'.\n  The results are shown for minv=",maxv," and maxv=", minv,".", sep=""))
                exc=maxv
                maxv=minv
                minv=exc       
        }
        x50=(maxv-minv)/2+minv
        
        res=.ess(beta,x50,maxv)
        test.in=res[1]*prop>=res[2]
        if (!test.in){
                for(i in seq(beta,2000*beta, by=beta*0.0001)){
                        test=.ess(beta=i,x50=x50,maxv=maxv)
                        if(test[1]*prop<=test[2]){
                                i
                                break 
                        }
                }
        }
        
        if (test.in){
                for(i in seq(beta/2000, beta, by=beta*0.0001)){
                        test=.ess(beta=i,x50=x50,maxv=maxv)
                        if(test[1]*prop>=test[2]){
                                i
                                break  
                        }
                }
        }
        angles=atan(.ess(beta=i,x50=x50,maxv=maxv))*180/pi
        out=data.frame(beta=i,alpha=-(i*x50),x50=x50,angle.x50=angles[1], min=minv,max=maxv,angle.infl=angles[2])
        out
}

.ess=function(beta,x50,maxv){
        dx2x=deriv(~ 1/(1+exp(-beta*(x-x50))), "x") 
        x <- x50
        res1=eval(dx2x)
        xx1=attr(res1, "gradient") 
        
        x <- maxv
        res2=eval(dx2x)
        xx2=attr(res2, "gradient") 
        res= c(xx1,xx2)
        res        
}



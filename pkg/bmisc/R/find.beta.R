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


find.beta=function( minv, maxv,prob=NULL, prop=0.1,beta=0.2, fast=TRUE){
        
        if(minv>maxv){
                warning(paste("Be sure that 'minv' is a smaller value than 'maxv'.\n  The results are shown for minv=",maxv," and maxv=", minv,".", sep=""))
                exc=maxv
                maxv=minv
                minv=exc       
        }
        x50=(maxv-minv)/2+minv
        nrep=2
        if(fast)nrep=1
        if(is.null(prob)){
                
                for(j in 1:nrep){
                        
                        res=.ess(beta,x50,maxv)
                        test.in=res[1]*prop>=res[2]
                        if (!test.in){
                                for(i in seq(beta,2000*beta, by=beta*0.0001)){
                                        test=.ess(beta=i,x50=x50,maxv=maxv)
                                        if(test[1]*prop<=test[2]){
                                                beta=i
                                                beta
                                                break 
                                        }
                                }
                        }
                        
                        if (test.in){
                                for(i in seq(beta, beta/2000,by=beta*-0.0001)){
                                        test=.ess(beta=i,x50=x50,maxv=maxv)
                                        if(test[1]*prop>=test[2]){
                                                beta=i
                                                beta
                                                break  
                                        }
                                }
                        }
                }
        }else{
                for(j in 1:nrep){
                        
                        res=.esp(beta,x50,maxv)
                        if(prob<0 & abs(prob)>1){
                                warning("You have specified 'prob' to be ", prob,".\n  It should be in [0,1]. The value used is ", abs(prob)/100,".", sep="")
                                prob=abs(prob)/100                        
                        }
                        if(prob<0 & abs(prob)<1){
                                warning("You have specified 'prob' to be ", prob,".\n  It should be in [0,1]. The value used is ", abs(prob),".", sep="")
                                prob=abs(prob)                        
                        }
                        if(prob>1){
                                warning("You have specified 'prob' to be ", prob,".\n  It should be in [0,1]. The value used is ", prob/100,".", sep="")
                                prob=prob/100                        
                        }
                        
                        test.in=  res <= prob
                        if (test.in){
                                for(i in seq(beta,2000*beta, by=beta*0.0001)){
                                        test=.esp(beta=i,x50=x50,maxv=maxv)
                                        if(test>=prob){
                                                beta=i
                                                beta
                                                break 
                                        }
                                }
                        }
                        
                        if (!test.in){
                                for(i in seq( beta,beta/2000, by=beta*-0.0001)){

                                        test=.esp(beta=i,x50=x50,maxv=maxv)
                                        if(test<=prob){
                                                beta=i
                                                beta
                                                break  
                                        }
                                }
                        }
                }
        }
        
        
        
        
        
        angles=atan(.ess(beta=beta,x50=x50,maxv=maxv))*180/pi
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

.esp=function(beta,x50,maxv){
        px1=1/(1+exp(-beta*(maxv-x50)))
        px1        
}



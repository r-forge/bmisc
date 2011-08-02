#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##     Plotting function for maturity ogive based on glm; binomial(logit)      ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-08-02															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################

plot.mat=function(object, ylab="Probability",xlab="Longueur à la fourche (mm)", se.pred=TRUE, legend=TRUE){
    
    temp=model.frame(result1)
    new.dat=data.frame(seq(min(temp[2], na.rm=T),max(temp[2], na.rm=T), by=0.1))
    names(new.dat)=names(temp)[2]
    
    fit=predict(result1,newdata=new.dat,se.fit=T, type="response")
    fit.link=predict(result1,newdata=new.dat,se.fit=T, type="link")
    inv.logit=function(x){
        family(result1)$linkinv(x)
    }
    fit.se=data.frame(upper=inv.logit(fit.link$fit+fit.link$se.fit*1.96), lower=inv.logit(fit.link$fit-fit.link$se.fit*1.96))
    
    
    plot(temp[,2], fitted(result1), type='n', col='blue', lwd=2,
            ylab=ylab, xlab=xlab )

    
    title("Régression logistique", cex.main=1.5, line=2)
    title("(± intervale de confiance à 95%)", cex.main=1, line=1)
    
    if(se.pred){
        polygon(c(new.dat$fourche,rev(new.dat$fourche)), c(fit.se$upper,rev(fit.se$lower)), col=gray(0.9), border=NA)
    }
    points(fit$fit~new.dat$fourche, type="l")
    points(code~fourche, data=mat)
    
    if(legend){
        val1 <-substitute(Mat[est]==over(ttt,1+e^{-(alpha+beta*L)}),list(ttt=1))
        val2 <- substitute(~~~~~~~~~~beta == b, list(b = round(coeffs[2],digits=2)))
        val3 <- substitute(~~~~~~~~~~alpha == a, list(a = round(coeffs[1],digits=10)))
        
        legend("topleft",legend=c(do.call("expression",list(val1)),
                        do.call("expression",list(val2)),
                        do.call("expression",list(val3))),
                bty='n', adj=0,y.intersp=c(0,1.8,1), cex=c(1.3,0.9,0.9))
    }
    

    
    L50=inv.pred(result1, p = c(0.5))
    lines(c(0,L50$est,L50$est), c(L50$prob,L50$prob,-1), lty=2)
    
    text(x=L50$est, L50$prob, expression(L[50]), pos=4, cex=1.2, font=3)
    
    text(x=L50$est, L50$prob, paste(round(L50$est, digits=2),"(±", round(1.96*L50$se, digits=2),")", sep=""), 
            srt=270, adj=c(-0.5,-0.5),cex=0.8, font=2)
    
}



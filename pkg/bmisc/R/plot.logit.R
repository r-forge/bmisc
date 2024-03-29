#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##     Plotting function for maturity ogive based on glm; binomial(logit)      ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-08-02															                               ##
## TODO AJOUTER UNE PARTIE QUI PERMETE DE PRENDRE LES R�SULTATS DE find.beta() ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################

plot.logit=function(object, 
                    ylab="Probability",
                    xlab="Length", 
                    se.pred=TRUE, 
                    leg=TRUE, 
                    ref=TRUE, 
                    range.x=c("data","full"), 
                    warn.val=TRUE, 
                    main,...)
{


        temp=model.frame(object)
        r.temp.x= range(temp[[2]])
        int.temp.x=r.temp.x[2]-r.temp.x[1]
        range.x=match.arg(range.x,range.x)
        switch(range.x,
                data= new.dat<-data.frame(seq(min(temp[2], na.rm=T),max(temp[2], na.rm=T), by=0.1)),
                full= new.dat<-data.frame(seq(
                                min(c(temp[[2]],inv.pred(object, y = 0.01)$est), na.rm=T),
                                max(c(temp[[2]],inv.pred(object, y = 0.99)$est), na.rm=T), by=0.1))
        )

        names(new.dat)=names(temp)[2]
        coeffs=coef(object)
        fit=predict(object,newdata=new.dat,se.fit=T, type="response")
        fit.link=predict(object,newdata=new.dat,se.fit=T, type="link")
        inv.link=function(x){
                family(object)$linkinv(x)
        }
        fit.se=data.frame(upper=inv.link(fit.link$fit+fit.link$se.fit*1.96), lower=inv.link(fit.link$fit-fit.link$se.fit*1.96))
        
        

        
        if(missing(main)){
                plot(x=temp[,2], y=fitted(object), type='n', col='blue', lwd=2,
                ylab=ylab, xlab=xlab , ylim=c(0,1), xlim=range(new.dat),...)
                        title("Logistic regression", cex.main=1.5, line=2)
                        title("(± confidence interval)", cex.main=1, line=1)

        }else{
                plot(x=temp[,2], y=fitted(object), type='n', col='blue', lwd=2,
                ylab=ylab, xlab=xlab , ylim=c(0,1), xlim=range(new.dat),main=main,...)
                }
        if(se.pred){
                polygon(c(new.dat[,1],rev(new.dat[,1])), c(fit.se$upper,rev(fit.se$lower)), col=gray(0.9), border=NA)
        }
        points(fit$fit~new.dat[,1], type="l")
        points(temp[,1]~temp[,2])
        
        if(ref){
                L50=inv.pred(object, y = 0.5)
                lines(c(0,L50$est,L50$est), c(L50$y,L50$y,-1), lty=2)
               
                L90=inv.pred(object, y = 0.90)
                lines(c(0,L90$est,L90$est), c(L90$y,L90$y,-1), lty=2)                
                
                L10=inv.pred(object, y = 0.10)
                lines(c(0,L10$est,L10$est), c(L10$y,L10$y,-1), lty=2)
                

                        text(x=L50$est, L50$y, expression(L[50]), pos=4, cex=1.2, font=3)                
                        text(x=L50$est, L50$y, paste(round(L50$est, digits=2),"(±", round(1.96*L50$se, digits=2),")", sep=""), 
                                srt=0, adj=c(1.2,-0.5),cex=0.8, font=2)
                        
                        text(x=L90$est, L90$y, expression(L[90]), pos=4, cex=1.2, font=3)                
                        text(x=L90$est, L90$y, paste(round(L90$est, digits=2)," (±", round(1.96*L90$se, digits=2),")", sep=""), 
                                srt=0, adj=c(1.2,-0.5),cex=0.8, font=2)
                        
                        text(x=L10$est, L10$y, expression(L[10]), pos=4, cex=1.2, font=3)               
                        text(x=L10$est, L10$y, paste(round(L10$est, digits=2),"(±", round(1.96*L10$se, digits=2),")", sep=""), 
                                srt=0, adj=c(1.2,-0.5),cex=0.8, font=2)

                
                if(any(c(L50$est,L90$est,L10$est)>par("usr")[2])){
                        warning(call.=FALSE,"Use option range.x='full' to show L levels correctly.")
                }
        }
        
        if(leg){
                limits=par('usr')
                x.leg1=limits[1]+((limits[2]-limits[1])*0.02)
                y.leg1=limits[3]+((limits[4]-limits[3])*0.90)
                
                val1 <-substitute(Mat[est]==over(ttt,1+e^{-(alpha+beta*L)}),list(ttt=1))
                val2 <- substitute(~~~~~~~~~~beta == ~~b, list(b = formatC(coeffs[2],digits=8, format="f")))
                val3 <- substitute(~~~~~~~~~~alpha == a, list(a = formatC(coeffs[1],digits=8, format="f")))
                
#        mtext(do.call("expression",list(val1)), line=-5, cex=1.3, adj=0.05)
#        mtext(do.call("expression",list(val2)), line=-7, cex=0.9, adj=0)
#        mtext(do.call("expression",list(val3)), line=-8, cex=0.9, adj=0)
                
                leg1=legend(x=x.leg1, y=y.leg1,legend=do.call("expression",list(val1)),
                        x.intersp=0,y.intersp=c(1.1),bty='n',cex=1.5)
                
                legend(x=leg1$rect$left, y=leg1$rect$top-leg1$rect$h,
                        legend=c(do.call("expression",list(val2)),do.call("expression",list(val3))),
                        x.intersp=c(0,0),y.intersp=c(1.5,1),bty='n',cex=1)
                
        }
        
        t.fit<-c(inv.pred(object, y = 0.01)$est,inv.pred(object, y = 0.99)$est)
        
        if(t.fit[1]< 0  | t.fit[2]> r.temp.x[2]+int.temp.x*0.2 ){
                warning(call.=FALSE,paste("Model fit might not be good.\n     L01=",
                                round(inv.pred(object, y = 0.01)$est,digits=2),
                                " and L99=",
                                round(inv.pred(object, y = 0.99)$est,digits=2),
                                " while\n     min(data)=",
                                min(temp[2]),
                                " and max(data)=",
                                max(temp[2]),"\nChoose warn.val=FALSE to remove warning indicators in plot.", sep=""))
                if(warn.val){
                        w01=inv.pred(object, y = 0.01)
                        w99=inv.pred(object, y = 0.99)
                        lines(x=rep(w01$est,2), y=c(w01$y, -0.15),xpd=TRUE, col="red",lty=4, lwd=2)
                        lines(x=rep(w99$est,2), y=c(w99$y, -0.15),xpd=TRUE, col="red",lty=4, lwd=2)
                        text(x=c(w01$est,w99$est), y=rep(-0.18,2), labels=c(expression(L["01"]),expression(L["99"])), col="red",cex=2,xpd=TRUE)
                        points(x=c(w01$est,w99$est),y=c(w01$y,w99$y), bg="red", pch=21, cex=1.5)
                        
                        
                }
        }  
        
        
        
}


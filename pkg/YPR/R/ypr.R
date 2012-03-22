#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##     Length based Yield per Recruit (YPR) model                              ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-07-15                                                            ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################

ypr <- function(LW, vonB, l.start, last.age, age.step=1, prop.surv=NULL , fish.lim=NULL ,
        Fsel.type=NULL, F.max=2, F.incr.YPR=0.0001,Mat=NULL,  Msel.type=NULL,
        M=0.2, f.MSP=0.4, rivard=FALSE, month=NULL){

        parms=list(LW=LW, vonB=vonB, last.age=last.age, l.start=l.start, age.step=age.step, prop.surv=prop.surv, fish.lim=fish.lim,
                Fsel.type=Fsel.type, F.max=F.max, F.incr.YPR=F.incr.YPR, Mat=Mat,
                Msel.type=Msel.type, M=M,  f.MSP=f.MSP, rivard=rivard, month=month)

        if(rivard & is.null(month)){
                warning("'month' was not specified.\n   Default value (6) will be used in natural mortality (M) adjustments to stock size.",call.=F)
                month=6
        }

        cl.vb=class(vonB)
        cl.LW=class(LW)
        cl.M=class(M)

        cl.vb=match.arg(cl.vb,c("numeric","nls"))
        cl.LW=match.arg(cl.LW,c("numeric","nls","lm"))
        if(cl.M=="character")M=match.arg(M,c("CW", "J","MG"))
        if(cl.M=="numeric" & M<0)stop("'M' can only take positive values.")

        if(cl.vb=="numeric" && all(names(vonB) %in% c("Linf","K","t0"))==FALSE ) stop(paste("When 'vonB' is a numeric vector, the names used for each value\n       should be c('Linf','K','t0') or c('Linf','K'). Current names are ",deparse(names(vonB)),".",sep=""))
        if(cl.vb=="numeric" & length(vonB)==3){vB.test=TRUE}
        if(cl.vb=="numeric" & length(vonB)==2){vB.test=FALSE}
        if(cl.vb=="nls" & length(coef(vonB))==3){vB.test=TRUE}
        if(cl.vb=="nls" & length(coef(vonB))==2){vB.test=FALSE}
        
        age.1=0
        if(cl.vb=="nls"& vB.test){
                coeffs=coef(vonB)
                Linf=coeffs[names(coeffs)=="Linf"]
                K=coeffs[names(coeffs)=="K"]
                t0=coeffs[names(coeffs)=="t0"]
                age.1=(log(-l.min.f/Linf+1)/-K)+ t0
        }
        
        if(cl.vb=="nls"& !vB.test){
                coeffs=coef(vonB)
                Linf=coeffs[names(coeffs)=="Linf"]
                K=coeffs[names(coeffs)=="K"]
        }
        if(class(vonB)=="numeric" & vB.test){
                Linf=vonB[names(vonB)=="Linf"]
                K=vonB[names(vonB)=="K"]
                t0=vonB[names(vonB)=="t0"]
                age.1=(log(-l.min.f/Linf+1)/-K)+ t0

        }


        age=seq(age.1,last.age+age.1,by=age.step)
        age=as.integer(age*1000000)
        age=age/1000000
        if(rivard & age.step!=1)age1=min(age): (last.age+age.1)

        switch(cl.vb,
                numeric= {
                        if(vB.test){
                                #names(vonB)=c("Linf","K","t0")
                                l.age<-vonB[1]*(1-exp(-vonB[2]*(age-t0))) #+ l.start*exp(-vonB[2]*age)
                                if(rivard & age.step!=1)l.age1<-vonB[1]*(1-exp(-vonB[2]*(age1-t0)))
                        }
                        if(!vB.test){
                                #names(vonB)=c("Linf","K","t0")
                                l.age<-vonB[1]*(1-exp(-vonB[2]*age)) + l.start*exp(-vonB[2]*age)
                                if(rivard & age.step!=1)l.age1<-vonB[1]*(1-exp(-vonB[2]*age1)) + l.start*exp(-vonB[2]*age1)
                        }
                },
                nls={   if(vB.test){
                          l.age<-Linf*(1-exp(-K*(age-t0)))
                          if(rivard & age.step!=1)l.age1<-Linf*(1-exp(-K*(age1-t0)))
                         }  
                        if(!vB.test){
                           l.age<-Linf*(1-exp(-K*(age)))+ (l.start*exp(-K*age))
                           if(rivard & age.step!=1)l.age1<-Linf*(1-exp(-K*(age1)))+ l.start*exp(-K*age1)
                         }
                }
        )

        switch(cl.LW,
                numeric= {
                        names(LW)=c("alpha","beta")
                        p.age=LW[1]*l.age^LW[2]
                        if(rivard & age.step!=1)p.age1=LW[1]*l.age1^LW[2]
                },
#                nls={
#                        coeffs<-coef(LW)
#                        
#                        alpha=coeffs[names(coeffs)=="alpha"]
#                        beta=coeffs[names(coeffs)=="beta"]
#                        LW2=c(alpha,beta)
#                        p.age=(alpha*l.age^beta)*cor
#                        if(rivard & age.step!=1)p.age1=alpha*l.age1^beta
#                },
                lm={
                        coeffs<-coef(LW)
                        cor=exp(sqrt(  (sum(residuals(LW)^2)) /(n(residuals(LW))-2))^2/2)      ### Sprugel 1983
                        alpha=exp(coeffs[1])
                        beta=coeffs[2]
                        LW2=c(alpha,beta)
                        p.age=cor*alpha*(l.age^beta)
                        if(rivard & age.step!=1)p.age1=alpha*l.age1^beta
                }
        )



        YPR=data.frame(age,l.age, p.age)
        if(rivard & age.step!=1)YPR1=data.frame(age1,l.age1, p.age1)
        if(rivard){
                if(age.step!=1){
                      YPR=cbind(YPR,.rivard(dat=YPR1,age.step=age.step, last.age=last.age, LW=LW, vonB=vonB)[,c(2,3)])
                }else{
                      YPR=merge(YPR,.rivard(dat=YPR,age.step=age.step, last.age=last.age, LW=LW, vonB=vonB)[,c(2,3)])
                      }
        }



        if(!is.null(fish.lim)) if(fish.lim>max(l.age))warning(paste("'fish.lim' (",fish.lim,") can not be higher than the length at 'last.age' (",round(max(l.age),digits=2),").
                                                Decrease 'fish.lim' or increase 'last.age'.",sep=""),call.=FALSE)


        F.i=seq(0,F.max, by=F.incr.YPR)
        F.i=as.integer( F.i*1000000)
        F.i=F.i/1000000
        n.F=n(F.i)

        ##############################################################################
        ##                               Maturity                                   ##
        ##############################################################################
        mat=.selectivity(sel.type=Mat,x=YPR$l.age)

        ##############################################################################
        ##                                  Msel.type                               ##
        ##############################################################################
        if(!is.null(M))M.sel=.selectivity(Msel.type,YPR$l.age)

        ##############################################################################
        ##                             F selectivity                                ##
        ##############################################################################
        F.sel=.selectivity(sel.type=Fsel.type,YPR$l.age)

        ##############################################################################
        ##                             FISHERMEN selectivity                        ##
        ##############################################################################
        if(!is.null(prop.surv)){
                if(is.null(fish.lim))stop("'fish.lim' needs to have a values when 'prop.surv' is present. Read help('ypr.l').")
                sel=which(YPR$l.age<fish.lim)
                prop.=1-prop.surv(YPR$l.age)
                if(any(prop.<0) & any(prop.>1))stop("'prop.surv' is not a function that expresses probabilities between [0,1].")
                F.sel[sel]=F.sel[sel]*prop.[sel]
        }

        ##############################################################################
        ##                                 Calculations                             ##
        ##############################################################################

        mat.frame=matrix(ncol=n.F, nrow=n(F.sel))

        ###  Mortality  ###
        F.=matrix(rep(F.sel,n.F),ncol=n.F, nrow=n(F.sel))
        F.=sweep(F.,MARGIN=2,F.i,`*`)

        switch(cl.M,
                character= {
                        switch(M,
                               CW= {if(!vB.test){
                                        M.all=m.j(k=K)
                                        parms$M="J"
                                        warning("'M' can not be estimated by 'CW' method since vonB for relative age does not estimate 't0'.\n   'J' method has been used instead.",call.=F)
                                        }else{M.all=m.cw(k=K,t0=t0,data=YPR)}},
                               MG= {M.all=m.mg(data=YPR)*M.sel},
                               J=  {M.all=m.j(k=K)*M.sel}
                            
                )},
                numeric= {M.all=M*M.sel}
        )


        Z=sweep(F.,MARGIN=1,M.all,`+`)
        Z1=colSums(Z, na.rm=TRUE)

        YPR$Mt=M.all
        YPR$F.sel=F.sel

        ###  Stock size  ###
        n.stock= mat.frame
        n.stock2= mat.frame
        if(rivard){
            n.stock[1,]=age.step
            switch(cl.M,
                    character= {n.stock2[1,]=age.step*exp(-((month-1)/12)*M.all[1])},
                    numeric=   {n.stock2[1,]=age.step*exp(-((month-1)/12)*M)}
                    )
            }else{
            n.stock[1,]=age.step
            n.stock2[1,]=age.step
            }

        for(i in 1:(n(F.sel)-1)){
                n.stock[i+1,]=n.stock[i,]*exp(-age.step*Z[i,])
                n.stock2[i+1,]=n.stock2[i,]*exp(-age.step*Z[i,])
        }
        n.stock1=colSums(n.stock, na.rm=TRUE)
        #n.stock22=colSums(n.stock2, na.rm=TRUE)

        ###  Biomass  ###
        if(rivard){pds.stock=sweep(n.stock,MARGIN=1,YPR$p.age.stk,FUN="*")
        }else{pds.stock=sweep(n.stock,MARGIN=1,YPR$p.age,FUN="*")}

        pds.stock1=colSums(pds.stock, na.rm=TRUE)
        pds.stock.moy=pds.stock1/n.stock1

        ###  Catches  ###
        #stop("Correct catch without smaller values of fish.lim !!!")
        F.p=F.
        if(!is.null(fish.lim)){
                sel2=which(YPR$l.age<fish.lim)
                F.p[sel2,]=F.p[sel2,]*0
        }
        n.catch=F.p/(Z)*(1-exp(-age.step*Z))* n.stock2
        n.catch[1,]=NA
        n.catch1=colSums(n.catch, na.rm=TRUE)

        ###  Catch weight  ###
        #pds.catch=n.catch
        pds.catch=sweep(n.catch,MARGIN=1,YPR$p.age,"*")
        pds.catch1=colSums(pds.catch, na.rm=TRUE)*(1/age.step)

        ###  Spawning Stock size  ###
        ssn=sweep(n.stock,MARGIN=1,mat,FUN="*")
        ssn1=colSums(ssn, na.rm=TRUE)

        ###  Spawning Stock biomass  ###
        if(rivard){ssb=sweep(ssn,MARGIN=1,YPR$p.age.stk,"*")
        }else{ssb=sweep(ssn,MARGIN=1,YPR$p.age,"*")}

        ssb1=colSums(ssb, na.rm=TRUE)

        ###  Maximum Spawning Potential  ###
        msp1=ssb1/max(ssb1)*100

        ###  Average length  ###
        if(rivard){l.moy=sweep(n.stock,MARGIN=1,YPR$l.age.stk,"*")
        }else{l.moy=sweep(n.stock,MARGIN=1,YPR$l.age,"*")}
        l.moy1=colSums(l.moy, na.rm=TRUE)/n.stock1

        ###  Average weight  ###
        if(rivard){p.moy=sweep(n.stock,MARGIN=1,YPR$p.age.stk,"*")
        }else{p.moy=sweep(n.stock,MARGIN=1,YPR$p.age,"*")}
        p.moy1=colSums(p.moy, na.rm=TRUE)/n.stock1

        ###  Average age  ###
        age.moy=sweep(n.stock,MARGIN=1,YPR$age,"*")
        age.moy1=colSums(age.moy, na.rm=TRUE)/n.stock1



        ##############################################################################
        ##                          Tableau de YPR vs F.i                           ##
        ##############################################################################

        YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1,stock.w=pds.stock1,
                ssn=ssn1,ssb=ssb1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)



        ##############################################################################
        ##                     Tableau des points de references                     ##
        ##############################################################################
        f.MSP.name=paste('F',round(f.MSP*100,digits=0),sep=".")
        ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA, avr.L=NA, avr.wgt=NA, avr.age=NA, f.MSP=NA)

        ### F0
        sel1=which(F.i==0)
        ref.table[1,]=c(F.i[sel1],pds.catch1[sel1], ssb1[sel1],pds.stock1[sel1], l.moy1[sel1],p.moy1[sel1],age.moy1[sel1], msp1[sel1])

        ### FMAX
        sel3=which(pds.catch1==max(pds.catch1))
        ref.table[3,]=c(F.i[sel3],pds.catch1[sel3], ssb1[sel3],pds.stock1[sel3], l.moy1[sel3],p.moy1[sel3],age.moy1[sel3], msp1[sel3])

        ###F01
        n.lm=length(pds.catch1)
        lm.mod=vector()
        for(i in 1:(n.lm-1)) lm.mod[i]=(pds.catch1[i+1]-pds.catch1[i])/(F.i[i+1]-F.i[i])

        pentes=abs(lm.mod-(lm.mod[1]*0.1))
        sel2=which(pentes==min(pentes))+1

        ref.table[2,]=c(F.i[sel2],pds.catch1[sel2], ssb1[sel2],pds.stock1[sel2], l.moy1[sel2],p.moy1[sel2],age.moy1[sel2], msp1[sel2])

        ### FMSP
        sel4=which(abs(msp1-f.MSP*100)==min(abs(msp1-f.MSP*100)))

        ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb1[sel4],pds.stock1[sel4], l.moy1[sel4],p.moy1[sel4],age.moy1[sel4], msp1[sel4])


        r.names=c("F.zero","F.01","F.max",f.MSP.name)
        row.names(ref.table)=r.names

        ref.line.sel=data.frame(line.sel=c(sel2,sel4, sel3))
        rownames(ref.line.sel)=r.names[c(2,4,3)]

        res=new("ypr",
                parms=parms,
                base=YPR,
                refs=ref.table,
                YPR=YPR.table)

        res

}


setClass("ypr",
        representation(
                parms="list",
                base="data.frame",
                refs="data.frame",
                YPR="data.frame")
)


setMethod("show", "ypr",
        function(object){
                ref.names=rownames(object@refs)
                dat=object@refs[,1]
                names(dat)=ref.names
                print(dat)
        }
)

setMethod("summary", "ypr",
        function(object){

                title= "Length based Yield per Recruit"


                # Title:
                cat(title)


                #VonB parameters:

                cl.vb=class(object@parms$vonB)
                cl.LW=class(object@parms$LW)
                cl.M=class(object@parms$M)
                switch(cl.vb,
                        numeric= {if(n(object@parms$vonB)==3){
                                      cat("\n\n - von Bartalanffy growth parameters:\n    Linf=",object@parms$vonB[[1]], "  K=",object@parms$vonB[[2]] , "  t0=",object@parms$vonB[[3]] ,sep = "")
                                      }else{
                                      cat("\n\n - von Bartalanffy growth parameters:\n    Linf=",object@parms$vonB[[1]], "  K=",object@parms$vonB[[2]] , sep = "")
                                      }
                        },
                        nls=     {if(n(coef(object@parms$vonB))==3){
                                      cat("\n\n - von Bartalanffy growth parameters:\n    Linf=", coef(object@parms$vonB)[[1]], "  K=",coef(object@parms$vonB)[[2]], "  t0=",coef(object@parms$vonB)[[3]]  , sep = "")
                                      }else{
                                      cat("\n\n - von Bartalanffy growth parameters:\n    Linf=", coef(object@parms$vonB)[[1]], "  K=",coef(object@parms$vonB)[[2]], sep = "")                                
                                      }
                        }
                )


                #LW parameters:
                switch(cl.LW,
                        numeric= {
                                cat("\n\n - Length-Weight curve parameters:\n    log(alpha)=", log(object@parms$LW[[1]]), "  beta=",object@parms$LW[[2]],"" , sep = "")
                        },
                        lm={
                                cat("\n\n - Length-Weight curve parameters:\n    log(alpha)=", coef(object@parms$LW)[[1]], "  beta=",coef(object@parms$LW)[[2]],"\n   >Correction for bias in log-transformed allometric function (Sprugel 1983)\n    has been used for weight estimates.", sep = "")
                        }
                )
                if(object@parms$rivard & object@parms$age.step==1) cat("\n   >Rivard Weight Calculations have been used.")
                if(object@parms$rivard & object@parms$age.step!=1) cat("\n   >Modified Rivard Weight Calculations have been used.")
                #M parameter
                switch(cl.M,
                        character= switch(object@parms$M,
                                CW= cat("\n\n - 'M' is estimated by Chen-Watanabe's (1989) age-dependent model."),
                                MG= cat("\n\n - 'M' is estimated by McGurk's (1986) model."),
                                J=cat("\n\n - 'M is estimated by Jensen's (1996) method.")
                        ),
                        numeric= {cat(paste("\n\n - Intantaneous natural mortality:\n    M =",object@parms$M))}
                )


                # Test Results:
                results = object@refs
                cat("\n\nResults:\n", sep = "")
                print(results)
        }
)


plot.ypr<-
        function(object,
                main='Yield per Recruit &\n SSB per Recruit',
                ylab.ypr='Yield per Recruit (YPR)',
                ylab.ssb='Spawning Stock Biomass per Recruit (SSB/R)',
                xlab='Fishing Mortality (F)',
                col.ypr='blue',
                col.ssb='red',
                ref=TRUE,
                legend=TRUE,
                ylim1,
                ylim2,
                xlim,
                add=FALSE){


        YPR=object@YPR
        refs=object@refs
        old.par=par()
        par(mar=c(5,4,4,4.1))
        col.lines=c(gray(0.4),gray(0),gray(0.6))

        if(!missing(ylim1)) {ylim1=ylim1}else{ylim1=c(0,max(YPR$ypr)*1.1)}
        if(!missing(ylim2)) {ylim2=ylim2}else{ylim2=c(0,max(YPR$ssb)*1.1)}
        if(!missing(xlim))  {xlim=xlim}else{xlim=range(YPR$F)}
        plot(YPR$ypr~YPR$F ,main=main,ylim=ylim1,
                ylab=ylab.ypr,xlab=xlab,type='l', lwd=3, col=col.ypr, las=1)

        if(ref){
                for(i in 2:dim(refs)[1]){
                        lines(c(-1,YPR)~c(F,F),data=refs[i,], lty=2)
                }

                lines(c(-1,YPR)~c(F,F),data=refs[2,], lty=2, col=col.lines[1])
                lines(c(-1,YPR)~c(F,F),data=refs[3,], lty=2, col=col.lines[2])
                lines(c(-1,YPR)~c(F,F),data=refs[4,], lty=1, col=col.lines[3])
                points(YPR~F,data=refs[2:dim(refs)[1],], pch=21, col='black', bg=col.lines,cex=1.2)
                y.coord=par('usr')[2]*0.01
                r.names=rownames(refs)
                par("usr")


        }

        par(new=TRUE)
        plot(YPR$ssb~YPR$F,type='l',xaxt="n",yaxt="n",xlab="",ylab="", lwd=3, col=col.ssb, ylim=ylim2)

        mtext(side=1, at=refs[2,1], line=-1, text=r.names[2], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=1.5, col=col.lines[1])
        mtext(side=1, at=refs[3,1], line=-1, text=r.names[3], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=1.5, col=col.lines[2])
        mtext(side=1, at=refs[4,1], line=-1, text=r.names[4], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=-0.3, col=col.lines[3])

        if(ref)points(SSB.R~F,data=refs[2:dim(refs)[1],], pch=21, col='black', bg=col.lines,cex=1.2)

        axis(4, las=1)
        mtext(ylab.ssb,side=4,line=2.9)
        if(legend){
                legend("topright",col=c("blue","red"),lty=1, lwd=3,legend=c("YPR","SSB/R"),
                        horiz=TRUE, bty='n', bg='white', seg.len=1)
        }
        par=old.par
}

plot.sel <- function (object, select=c(1,2,3),leg=TRUE,...) UseMethod("plot.sel")

plot.sel.ypr <- function(object, main, xlab, ylab, select=c(1,2,3),leg=TRUE){

        if(missing(main)) main=c("Maturity\n(Mat)", "Natural Mortality at length","Fishing Selectivity\n(Fsel.type)")
        if(missing(xlab)) xlab="Length"
        if(missing(ylab))  ylab="Probability"
        
        vonB=object@parms$vonB
        LW=object@parms$LW
        M=object@parms$M
        
        cl.vb=class(vonB)
        cl.LW=class(LW)
        cl.M=class(M)
        
        if(cl.vb=="numeric" && all(names(vonB) %in% c("Linf","K","t0"))==FALSE ) stop(paste("When 'vonB' is a numeric vector, the names used for each value\n       should be c('Linf','K','t0') or c('Linf','K'). Current names are ",deparse(names(vonB)),".",sep=""))
        if(cl.vb=="numeric" & length(vonB)==3){vB.test=TRUE}
        if(cl.vb=="numeric" & length(vonB)==2){vB.test=FALSE}
        if(cl.vb=="nls" & length(coef(vonB))==3){vB.test=TRUE}
        if(cl.vb=="nls" & length(coef(vonB))==2){vB.test=FALSE}
        
######   Il faut partir des age pour les données utilisé sur le graph ##########  
        age.1=0
        if(cl.vb=="nls"& vB.test){
                coeffs=coef(vonB)
                Linf=coeffs[names(coeffs)=="Linf"]
                K=coeffs[names(coeffs)=="K"]
                t0=coeffs[names(coeffs)=="t0"]
                age.1=(log(-l.min.f/Linf+1)/-K)+ t0
        }
        
        if(cl.vb=="nls"& !vB.test){
                coeffs=coef(vonB)
                Linf=coeffs[names(coeffs)=="Linf"]
                K=coeffs[names(coeffs)=="K"]
        }
        if(class(vonB)=="numeric" & vB.test){
                Linf=vonB[names(vonB)=="Linf"]
                K=vonB[names(vonB)=="K"]
                t0=vonB[names(vonB)=="t0"]
                age.1=(log(-l.min.f/Linf+1)/-K)+ t0

        }

        xlim=c(min(object@base$l.age,na.rm=T),max(object@base$l.age,na.rm=T))
        #x.dat=seq(xlim[1],xlim[2], by=(xlim[2]-xlim[1])/1000)
        ylim=c(0,1)
        x.dat=seq(age.1,object@parms$last.age+age.1,by=0.01)


        switch(cl.vb,
                numeric= {
                        if(vB.test){
                                Linf=vonB[names(vonB)=="Linf"]
                                K=vonB[names(vonB)=="K"]
                                t0=vonB[names(vonB)=="t0"]
                                x.dat2<-vonB[1]*(1-exp(-vonB[2]*(x.dat-t0))) #+ l.start*exp(-vonB[2]*x.dat)

                        }
                        if(!vB.test){
                                Linf=vonB[names(vonB)=="Linf"]
                                K=vonB[names(vonB)=="K"]  
                                x.dat2<-vonB[1]*(1-exp(-vonB[2]*x.dat)) + (object@parms$l.start*exp(-vonB[2]*x.dat))

                        }
                },
                nls={   if(vB.test){
                                coeffs=coef(vonB)
                                Linf=coeffs[names(coeffs)=="Linf"]
                                K=coeffs[names(coeffs)=="K"]
                                t0=coeffs[names(coeffs)=="t0"]                
                                x.dat2<-Linf*(1-exp(-K*(x.dat-t0)))

                         }  
                        if(!vB.test){
                                coeffs=coef(vonB)
                                Linf=coeffs[names(coeffs)=="Linf"]
                                K=coeffs[names(coeffs)=="K"]
                                x.dat2<-Linf*(1-exp(-K*(x.dat)))+ (object@parms$l.start*exp(-K*x.dat))

                         }
                }
        )   
        
        x.dat.a=x.dat
        x.dat=x.dat2
        
        
        switch(cl.LW,
                numeric= {
                        names(LW)=c("alpha","beta")
                        x.dat.p=LW[1]*x.dat^LW[2]

                },
                lm={
                        coeffs<-coef(LW)
                        cor=exp(sqrt(  (sum(residuals(LW)^2)) /(n(residuals(LW))-2))^2/2)      ### Sprugel 1983
                        alpha=exp(coeffs[1])
                        beta=coeffs[2]
                        LW2=c(alpha,beta)
                        x.dat.p=cor*alpha*(x.dat^beta)

                }
        )
           

        
       
        ##############################################################################
        ##                               Maturity                                   ##
        ##############################################################################
        mat=.selectivity(object@parms$Mat, x.dat)
        #mat=object@base$Mt

        ##############################################################################
        ##                                     Mt                                   ##
        ##############################################################################
        M.sel=.selectivity(object@parms$Msel.type, x.dat)
######### il faut faire le calcul des poids à l'âge
        switch(cl.M,
                character= {
                        switch(object@parms$M,
                               CW= {if(!vB.test){
                                        M.all=m.j(k=K)*M.sel
                                        parms$M="J"
                                        warning("'M' can not be estimated by 'CW' method since vonB for relative age does not estimate 't0'.\n   'J' method has been used instead.",call.=F)
                                        }else{M.all=m.cw(k=K,t0=t0,data=x.dat.a)}},
                               MG= {M.all=m.mg(data=x.dat.p)*M.sel},
                               J=  {M.all=m.j(k=K)*M.sel}
                            
                )},
                numeric= {M.all=M*M.sel}
        )

        ##############################################################################
        ##                             F selectivity                                ##
        ##############################################################################
        F.sel=.selectivity(object@parms$Fsel.type, x.dat)

        ##############################################################################
        ##                             FISHERMEN selectivity                        ##
        ##############################################################################
        if(!is.null(object@parms$prop.surv)){
                if(is.null(object@parms$fish.lim))stop("'fish.lim' needs to have a values when 'prop.surv' is present. Read help('ypr.l').")
                sel=which(x.dat<object@parms$fish.lim)
                x.dat2=x.dat[sel]
                prop.=1-object@parms$prop.surv(x.dat)
                F.sel2=F.sel
                F.sel2[sel]=F.sel[sel]*prop.[sel]
        }
        if(length(select)==3){
                par(mfrow=c(2,2))
        }else{
                if(length(select)==2 & 3 %in% select){
                        par(mfrow=c(2,2))
                }else{
                        if(length(select)==2 & !(3 %in% select)){
                                par(mfrow=c(1,2))
                        }else{
                                if(length(select)==1 & 3 == select & leg){
                                        par(mfrow=c(1,2))
                                }else{
                                        if(length(select)==1 & 3 !=select){
                                                par(mfrow=c(1,1))
                                        }else{
                                                if(length(select)==1 & 3 ==select & !leg) par(mfrow=c(1,1))
                                        }
                                }
                        }
                }
        }
        if(1 %in% select){
                plot(mat~x.dat, xlim=xlim, ylim=ylim, main=main[1], type='l', lwd=2.7, xlab=xlab, ylab=ylab)
        }
        if(2 %in% select){
                plot(M.all~x.dat  , xlim=xlim, ylim=ylim, main=main[2], type='l', lwd=2.7, xlab=xlab, ylab=ylab)
        }
        if(3 %in% select){
                plot(F.sel~x.dat  , xlim=xlim, ylim=ylim, main=main[3], type='l', lwd=2.7, xlab=xlab, ylab=ylab)
                if(!is.null(object@parms$prop.surv)){
                        points(F.sel2~x.dat, type='l', lwd=2.5, col='blue' )
                        abline(v=object@parms$fish.lim, lwd=2.5, col="red")
                        if(leg){
                                plot(1,type="n", xaxt="n",yaxt="n", xlab="", ylab="", bty="n")
                                legend("topleft",col=c("red", "blue"),lty=1, lwd=3,legend=c("Minimum legal catch size","Fishing selectivity when\nconsidering survival of\nbycatch"),
                                        horiz=F, bty='n', bg='white', seg.len=1)
                        }
                }
        }
        par(mfrow=c(1,1))
}


m.cw=function(k,t0,data){
        
        if(is.data.frame(data)){t= data$age}else{t=data}

        mt1=k/(1-exp(-k*(t-t0)))

        tm=-(1/k)*log(abs(1-exp(k*t0)))+t0
        sel=which(t<tm)
        sel2=which(t>=tm)

        a0=1-exp(-k*(tm-t0))
        a1=k*exp(-k*(tm-t0))
        a2=-(1/2*k^2)*exp(-k*(tm-t0))

        mt2=k/(a0+(a1*(t-tm))+(a2*(t-tm)^2))

        #mmt=data
        Mt=c(mt1[sel],mt2[sel2])

        Mt
}


m.mg=function(data){
        if(is.data.frame(data)){t= data$p.age}else{t=data}
        Mt=0.00526*(t^(-0.25))
        Mt
}

m.j=function(k){
        Mt=1.5*(k)
        Mt
}


.rivard <- function(dat, pred=FALSE, K=2, plus.gr=FALSE,last.age,age.step, LW, vonB)
{
        n.age=dim(dat)[1]
        n.an=2
        pds=data.frame(dat[,3],dat[,3])
        pds1=log(pds)
        pds2=pds1


        if(!plus.gr){
                for(i in 2:n.age){
                        for(j in 2:n.an){
                                pds2[i,j]=(pds1[i,j]+pds1[i-1,j-1])/2
                        }
                }

                for(i in 1:(n.an-1)){
                        pds2[1,i]=2*pds1[1,i]-pds2[2,i+1]
                }

                for(i in 1:(n.age-1)){
                        pds2[i,1]=2*pds1[i,1]-pds2[i+1,2]
                }

                pds2[n.age,1]=(pds1[n.age,1]+pds1[n.age-1,1])/2

                pds2[1,n.an]=2*pds1[1,n.an]-pds2[2,n.an]

                pds3=exp(pds2)

                if(pred){
                        last.name=as.character((as.numeric(names(pds3)[n.an])+1))
                        pds3$pred.next=rowMeans(pds3[,(n.an-K+1):n.an])
                        names(pds3)[dim(pds3)[2]]=last.name
                }

        }else{
                for(i in 2:n.age-1){
                        for(j in 2:n.an){
                                pds2[i,j]=(pds1[i,j]+pds1[i-1,j-1])/2
                        }
                }

                for(i in 1:(n.an-1)){
                        pds2[1,i]=2*pds1[1,i]-pds2[2,i+1]
                }

                for(i in 1:(n.age-2)){
                        pds2[i,1]=2*pds1[i,1]-pds2[i+1,2]
                }

                pds2[n.age-1,1]=(pds1[n.age-1,1]+pds1[n.age-2,1])/2

                pds2[1,n.an]=2*pds1[1,n.an]-pds2[2,n.an]

                for(i in 1:n.an){
                        pds2[n.age,i]=pds1[n.age,i]
                }

                pds3=exp(pds2)
                if(pred){
                        last.name=as.character((as.numeric(names(pds3)[n.an])+1))
                        pds3$pred.next=rowMeans(pds3[,(n.an-K+1):n.an])
                        names(pds3)[dim(pds3)[2]]=last.name
                }

        }
        pds3=data.frame(dat[,1],pds3[,2])
        cl.LW=class(LW)
                switch(cl.LW,
                        numeric= {
                                names(LW)=c("alpha","beta")
                                pds3$l.age.stk=(pds3[,2]/LW[1])^(1/LW[2])
                        },
                        nls={
                                coeffs<-coef(LW)
                                alpha=coeffs[names(coeffs)=="alpha"]
                                beta=coeffs[names(coeffs)=="beta"]
                                LW2=c(alpha,beta)
                                pds3$l.age.stk=(pds3[,2]/alpha)^(1/beta)
                        },
                        lm={
                                coeffs<-coef(LW)
                                alpha=exp(coeffs[1])
                                beta=coeffs[2]
                                LW2=c(alpha,beta)
                                pds3$l.age.stk=(pds3[,2]/alpha)^(1/beta)
                        }
                )
        names(pds3)=c("age","p.age.stk","l.age.stk")

        if(age.step!=1){
        pds4=pds3
        age=seq(min(pds4$age),max(pds4$age),by=age.step)
        age=as.integer(age*1000000)
        age=age/1000000
        pds5=data.frame(age=age)
        if(length(coef(vonB))==3){
              la=nls(l.age.stk~  Linf*(1-exp(-K*((age)-t0))), data=pds4,start=as.list(coef(vonB)),
                      nls.control(maxiter = 1000, minFactor=0.00000000001)) 
        }
        if(length(coef(vonB))==2){
               la=nls(l.age.stk~  Linf*(1-exp(-K*age))+l.start*exp(-K*age), data=pds4,start=as.list(coef(vonB)),
                      nls.control(maxiter = 1000, minFactor=0.00000000001)) 
                      }
        
        pds5$l.age.stk=predict(object=la,newdata=pds5)
        switch(cl.LW,
                numeric= {
                        names(LW)=c("alpha","beta")
                        pds5$p.age.stk=LW[1]*pds5$l.age.stk^LW[2]
                },
                nls={
                        coeffs<-coef(LW)
                        alpha=coeffs[names(coeffs)=="alpha"]
                        beta=coeffs[names(coeffs)=="beta"]
                        LW2=c(alpha,beta)
                        pds5$p.age.stk=alpha*pds5$l.age.stk^beta
                },
                lm={
                        coeffs<-coef(LW)
                        alpha=exp(coeffs[1])
                        beta=coeffs[2]
                        LW2=c(alpha,beta)
                        pds5$p.age.stk=alpha*pds5$l.age.stk^beta
                }
        )
        pds3=pds5
        }
        return(pds3)

}



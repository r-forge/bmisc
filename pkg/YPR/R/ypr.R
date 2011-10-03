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
        M=0.2, f.MSP=0.4){
    
    parms=list(LW=LW, vonB=vonB, last.age=last.age, l.start=l.start, age.step=age.step, prop.surv=prop.surv, fish.lim=fish.lim,
            Fsel.type=Fsel.type, F.max=F.max, F.incr.YPR=F.incr.YPR, Mat=Mat,
            Msel.type=Msel.type, M=M,  f.MSP=f.MSP)
    
    cl.vb=class(vonB)
    cl.LW=class(LW)
    
    age=seq(0,last.age,by=age.step)
    age=as.integer(age*1000000)
    age=age/1000000
    switch(cl.vb,
            numeric= {
                names(vonB)=c("Linf","K")
                l.age<-vonB[1]*(1-exp(-vonB[2]*age)) + l.start*exp(-vonB[2]*age)
            },
            nls={
                coeffs<-coef(vonB)
                Linf=coeffs[names(coeffs)=="Linf"]
                K=coeffs[names(coeffs)=="K"]
                vonB=c(Linf, K)
                l.age<-Linf*(1-exp(-K*age)) + l.start*exp(-K*age)
            }
    )
    
    switch(cl.LW,
            numeric= {
                names(LW)=c("alpha","beta")
                p.age=LW[1]*l.age^LW[2]
            },
            nls={
                coeffs<-coef(LW)
                alpha=coeffs[names(coeffs)=="alpha"]
                beta=coeffs[names(coeffs)=="beta"]
                LW=c(alpha,beta)
                p.age=alpha*l.age^beta
            },
            lm={
                coeffs<-coef(LW)
                alpha=exp(coeffs[1])
                beta=coeffs[2]
                LW=c(alpha,beta)
                p.age=alpha*l.age^beta
            }
    )
    
    

    YPR=data.frame(age,l.age, p.age)

    
    F.i=seq(0,F.max, by=F.incr.YPR)
    F.i=as.integer( F.i*1000000)
    F.i=F.i/1000000
    n.F=n(F.i)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
    mat=.selectivity(Mat,YPR$l.age)
    
    ##############################################################################
    ##                                  Msel.type                               ##
    ##############################################################################
    M.sel=.selectivity(Msel.type,YPR$l.age)
    
    ##############################################################################
    ##                             F selectivity                                ##
    ##############################################################################
    F.sel=.selectivity(Fsel.type,YPR$l.age)
    
    ##############################################################################
    ##                             FISHERMEN selectivity                        ##
    ##############################################################################
    if(!is.null(prop.surv)){
        if(is.null(fish.lim))stop("'fish.lim' needs to have a values when 'prop.surv' is present. Read help('ypr.l').")
        sel=which(YPR$l.age<fish.lim)
        prop.=1-prop.surv(YPR$l.age)
        F.sel[sel]=F.sel[sel]*prop.[sel]
    }
    
    ##############################################################################
    ##                          Matrices des calculs                            ##
    ##############################################################################
    
    mat.frame=matrix(ncol=n.F, nrow=n(F.sel))
    
    F.=matrix(rep(F.sel,n.F),ncol=n.F, nrow=n(F.sel))
    F.=sweep(F.,MARGIN=2,F.i,`*`)
    
    M.all=M*M.sel
    Z=sweep(F.,MARGIN=1,M.all,`+`)
    
    Z1=colSums(Z, na.rm=TRUE)
    
    n.stock= mat.frame
    n.stock[1,]=1
    for(i in 1:(n(F.sel)-1)){
        n.stock[i+1,]=n.stock[i,]*exp(-Z[i,])
    }
    n.stock1=colSums(n.stock, na.rm=TRUE)
    
    pds.stock=sweep(n.stock,MARGIN=1,YPR$p.age,FUN="*")
    pds.stock1=colSums(pds.stock, na.rm=TRUE)
    
    pds.stock.moy=pds.stock1/n.stock1
    
    n.catch=F./(F.+M)* n.stock*(1-exp(-Z))
    n.catch[1,]=NA
    n.catch1=colSums(n.catch, na.rm=TRUE)
    
    pds.catch=n.stock
    pds.catch=sweep(n.catch,MARGIN=1,YPR$p.age,"*")
    pds.catch1=colSums(pds.catch, na.rm=TRUE)
    
    
    ssn=sweep(n.stock,MARGIN=1,mat,FUN="*")
    ssn1=colSums(ssn, na.rm=TRUE)
    
    ssb=sweep(ssn,MARGIN=1,YPR$p.age,"*")
    ssb1=colSums(ssb, na.rm=TRUE)
    
    msp1=ssb1/max(ssb1)*100
    
    l.moy=sweep(n.stock,MARGIN=1,YPR$l.age,"*")
    l.moy1=colSums(l.moy, na.rm=TRUE)/n.stock1
    
    p.moy=sweep(n.stock,MARGIN=1,YPR$p.age,"*")
    p.moy1=colSums(p.moy, na.rm=TRUE)/n.stock1
    
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
    ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA, avr.L=NA, avr.wgt=NA, avr.age=NA)
    
    ### F0
    sel1=which(F.i==0)
    ref.table[1,]=c(F.i[sel1],pds.catch1[sel1], ssb1[sel1],pds.stock1[sel1], l.moy1[sel1],p.moy1[sel1],age.moy1[sel1])
    
    ### FMAX
    sel3=which(pds.catch1==max(pds.catch1))
    ref.table[3,]=c(F.i[sel3],pds.catch1[sel3], ssb1[sel3],pds.stock1[sel3], l.moy1[sel3],p.moy1[sel3],age.moy1[sel3])
    
    ###F01
    n.lm=length(pds.catch1)
    lm.mod=vector()
    for(i in 1:(n.lm-1)) lm.mod[i]=(pds.catch1[i+1]-pds.catch1[i])/(F.i[i+1]-F.i[i])
    
    pentes=abs(lm.mod-(lm.mod[1]*0.1))
    sel2=which(pentes==min(pentes))+1
    
    ref.table[2,]=c(F.i[sel2],pds.catch1[sel2], ssb1[sel2],pds.stock1[sel2], l.moy1[sel2],p.moy1[sel2],age.moy1[sel2])
    
    ### FMSP
    sel4=which(abs(msp1-f.MSP*100)==min(abs(msp1-f.MSP*100)))
    
    ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb1[sel4],pds.stock1[sel4], l.moy1[sel4],p.moy1[sel4],age.moy1[sel4])

    
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
            cat("\nTitle:\n ",title, "\n", sep = "")
            
            #VonB parameters:
            
                cl.vb=class(object@parms$vonB)
                cl.LW=class(object@parms$LW)
    switch(cl.vb,
            numeric= {
cat("\nvon Bartalanffy growth parameters:\n Linf=",object@parms$vonB[[1]], "  K=",object@parms$vonB[[2]] , sep = "")
            },
            nls={
                cat("\nvon Bartalanffy growth parameters:\n Linf=", coef(object@parms$vonB)[[1]], "  K=",coef(object@parms$vonB)[[2]] , sep = "")
            }
    )

            
            #LW parameters:
            switch(cl.LW,
            numeric= {
cat("\nLength-Weight curve parameters:\n log(alpha)=", log(object@parms$LW[[1]]), "  beta=",object@parms$LW[[2]] , sep = "")
            },
            nls={
cat("\nLength-Weight curve parameters:\n log(alpha)=", log(coef(object@parms$LW)[[2]]), "  beta=",coef(object@parms$LW)[[1]] , sep = "")
            },
            lm={
cat("\nLength-Weight curve parameters:\n log(alpha)=", coef(object@parms$LW)[[1]], "  beta=",coef(object@parms$LW)[[2]] , sep = "")
            }
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
                legend=TRUE){
    
    
    YPR=object@YPR
    refs=object@refs
    
    par(mar=c(5,4,4,4.1))
    col.lines=c(gray(0.4),gray(0),gray(0.6))
    ylim1=c(0,max(YPR$ypr)*1.1)
    ylim2=c(0,max(YPR$ssb)*1.1)
    plot(YPR$ypr~YPR$F  ,main=main,ylim=ylim1, 
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

        mtext(side=1, at=refs[2,1], line=-1, text=r.names[2], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=1.5, col=col.lines[1])
        mtext(side=1, at=refs[3,1], line=-1, text=r.names[3], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=1.5, col=col.lines[2])
        mtext(side=1, at=refs[4,1], line=-1, text=r.names[4], cex=0.8, font=2, xpd=T, las=2,adj=0, padj=-0.3, col=col.lines[3])
    }
    
    par(new=TRUE)
    plot(YPR$ssb~YPR$F,type='l',xaxt="n",yaxt="n",xlab="",ylab="", lwd=3, col=col.ssb, ylim=ylim2)
    
    if(ref)points(SSB.R~F,data=refs[2:dim(refs)[1],], pch=21, col='black', bg=col.lines,cex=1.2)
    
    axis(4, las=1)
    mtext(ylab.ssb,side=4,line=2.9)
    if(legend){
        legend("topright",col=c("blue","red"),lty=1, lwd=3,legend=c("YPR","SSB/R"),
                horiz=TRUE, bty='n', bg='white', seg.len=1)
    }
}

plot.sel <- function (object) UseMethod("plot.sel")

plot.sel.ypr <- function(object, main, xlab, ylab){
    
    if(missing(main)) main=c("Maturity\n(Mat)", "Natural Mortality Selectivity\n(Msel.type)","Fishing Selectivity\n(Fsel.type)") 
    if(missing(xlab)) xlab="Length"
    if(missing(ylab))  ylab="Probability"
    
    xlim=c(min(object@base$l.age,na.rm=T),max(object@base$l.age,na.rm=T))
    x.dat=seq(xlim[1],xlim[2], by=(xlim[2]-xlim[1])/1000)
    ylim=c(0,1)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
    mat=.selectivity(object@parms$Mat, x.dat)
    
    ##############################################################################
    ##                                  Msel.type                               ##
    ##############################################################################
    M.sel=.selectivity(object@parms$Msel.type, x.dat)
    
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

    par(mfrow=c(2,2))
    plot(mat~x.dat, xlim=xlim, ylim=ylim, main=main[1], type='l', lwd=2.7, xlab=xlab, ylab=ylab)	
    plot(M.sel~x.dat  , xlim=xlim, ylim=ylim, main=main[2], type='l', lwd=2.7, xlab=xlab, ylab=ylab)
    plot(F.sel~x.dat  , xlim=xlim, ylim=ylim, main=main[3], type='l', lwd=2.7, xlab=xlab, ylab=ylab)
    if(!is.null(object@parms$prop.surv)){
        points(F.sel2~x.dat, type='l', lwd=2.5, col='blue' )
        abline(v=object@parms$fish.lim, lwd=2.5, col="red")
        plot(1,type="n", xaxt="n",yaxt="n", xlab="", ylab="", bty="n")
        legend("topleft",col=c("red", "blue"),lty=1, lwd=3,legend=c("Minimum legal catch size","Fishing selectivity when\nconsidering survival of\nbycatch"),
                horiz=F, bty='n', bg='white', seg.len=1)
    }
}



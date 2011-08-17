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

ypr.l <- function(LW, vonB, l.start, last.age, age.step=1, Fsel.type, F.max=2, F.incr.YPR=0.0001, 
                    M.l,  M=0.2, f.MSP=0.4, F.f=0, M.f=0.5, riv.calc=FALSE){
    
    parms=list(Fsel.type=Fsel.type,last.age=last.age, l.start=l.start,
            age.step=age.step, LW=LW, vonB=vonB, F.max=F.max, F.incr.YPR=F.incr.YPR,
            M=M, M.l=M.l, f.MSP=f.MSP, riv.calc=riv.calc, F.f=F.f, M.f=M.f)
    
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
    
    
    if(riv.calc){
        p.age.riv=rivard(data.frame(p.age,p.age),pred=FALSE,plus.gr=FALSE)[,2]
        YPR=data.frame(age,l.age, p.age, p.age.riv)
    }else{
        YPR=data.frame(age,l.age, p.age)
    }
    
    F.i=seq(0,F.max, by=F.incr.YPR)
    F.i=as.integer( F.i*1000000)
    F.i=F.i/1000000
    n.F=n(F.i)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
    if(class(M.l)[1] %nin$% c("list","glm")) stop("M.l has to be either a list or a glm (logit) object.")
    if(class(M.l)[1]=="list"){
        switch(M.l[[1]],
                full= mat.sel <- full.sel(sel.full=M.l[[2]], L = YPR$l.age),
                ramp= mat.sel <- ramp.sel(sel.zero =M.l[[2]], sel.full = M.l[[3]], L = YPR$l.age),
                logistic= mat.sel <- logistic.sel(alpha=M.l[[2]], beta=M.l[[3]], L=YPR$l.age)
        )   
    }
    
    if(class(M.l)[1]=="glm"){
        coeffs=coef(M.l)
        mat.sel <- 1/(1+exp(-(coeffs[[1]]+coeffs[[2]]*YPR$l.age)))
    }

    ##############################################################################
    ##                             F selectivity                                ##
    ##############################################################################
    if(class(Fsel.type)[1] %nin% c("list","glm")) stop("Fsel.type has to be either a list or a glm (logit) object.")
    if(class(Fsel.type)[1]=="list"){
        switch(Fsel.type[[1]],
                full= F.sel <- full.sel(sel.full=Fsel.type[[2]], L = YPR$l.age),
                ramp= F.sel <- ramp.sel(sel.zero =Fsel.type[[2]], sel.full = Fsel.type[[3]], L = YPR$l.age),
                logistic= F.sel <- logistic.sel(alpha=Fsel.type[[2]], beta=Fsel.type[[3]], L=YPR$l.age)
        )
    }
    if(class(Fsel.type)[1]=="glm"){
        coeffs=coef(Fsel.type)
        F.sel <- 1/(1+exp(-(coeffs[[1]]+coeffs[[2]]*YPR$l.age)))
    }
    
    ##############################################################################
    ##                             FISHERMEN selectivity                        ##
    ##############################################################################
    ## TODO ADD FISHERMEN SELECTIVITY CORRECTION ON F.SEL
    
    
    ##############################################################################
    ##                          Matrices des calculs                            ##
    ##############################################################################
    
    mat.frame=matrix(ncol=n.F, nrow=n(F.sel))
    
    F.=matrix(rep(F.sel,n.F),ncol=n.F, nrow=n(F.sel))
    F.=sweep(F.,MARGIN=2,F.i,`*`)
    
    Z=F.+M
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
    
    
    ssn=sweep(n.stock,MARGIN=1,mat.sel,FUN="*")
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
    ##            recalcul en fonction du premier janvier avec Rivard           ##
    ##############################################################################
    if(riv.calc){
        
        F.ts=sweep(F.,MARGIN=2,F.f,`*`)
        M.ts=M*M.f
        Z.ts=F.ts+M.ts
        
        n.stock.ts=n.stock*exp(-Z.ts)
        
        n.stock.ts1=colSums(n.stock.ts, na.rm=TRUE)
        
        pds.stock.ts=sweep(n.stock,MARGIN=1,YPR$p.age.riv,FUN="*")
        pds.stock.ts1=colSums(pds.stock.ts, na.rm=TRUE)
        pds.stock.ts1[201]
        
        W=n.stock.ts*pds.stock.ts
        Wts=sweep(W,MARGIN=1,mat.sel,FUN="*")
        Wts1=colSums(Wts, na.rm=TRUE)
        Wts1[1]-ssb1[1]
        
        
        ssn.ts=sweep(n.stock.ts,MARGIN=1,mat.sel,FUN="*")
        ssn.ts1=colSums(ssn.ts, na.rm=TRUE)
        ssn.ts1[1]
        
        ssb.ts=sweep(ssn.ts,MARGIN=1,YPR$p.age.riv,"*")
        ssb.ts1=colSums(ssb.ts, na.rm=TRUE)
        ssb.ts1[1]
        
        msp.ts1=ssb.ts1/max(ssb.ts1)*100
        msp.ts1[201]
        
        ##############################################################################
        ##                          Tableau de YPR vs F.i                           ##
        ##############################################################################
        YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock.ts1,stock.w=pds.stock.ts1, 
                ssn=ssn.ts1,ssb=ssb.ts1,f.MSP=msp.ts1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
        
        ##############################################################################
        ##                       Tableau de YPR vs F.i Resume                       ##
        ##############################################################################
        
        F.i2=seq(0,F.max, by=0.01)
        F.i2=as.integer( F.i2*1000000)
        F.i2=F.i2/1000000
        
        sel.Fi=which(YPR.table$F %in% F.i2)
        YPR.table.short=YPR.table[sel.Fi,]
        
        
        ##############################################################################
        ##                     Tableau des points de references                     ##
        ##############################################################################
        f.MSP.name=paste('F',round(f.MSP*100,digits=0),sep=".")
        ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA, avr.L=NA, avr.wgt=NA, avr.age=NA)
        
        ### F0
        sel1=which(F.i==0)
        ref.table[1,]=c(F.i[sel1],pds.catch1[sel1], ssb.ts1[sel1],pds.stock.ts1[sel1], l.moy1[sel1],p.moy1[sel1],age.moy1[sel1])
        
        ### FMAX
        sel3=which(pds.catch1==max(pds.catch1))
        ref.table[3,]=c(F.i[sel3],pds.catch1[sel3], ssb.ts1[sel3],pds.stock.ts1[sel3], l.moy1[sel3],p.moy1[sel3],age.moy1[sel3])
        
        ###F01
        n.lm=length(pds.catch1)
        lm.mod=vector()
        for(i in 1:(n.lm-1)) lm.mod[i]=(pds.catch1[i+1]-pds.catch1[i])/(F.i[i+1]-F.i[i])
        
        pentes=abs(lm.mod-(lm.mod[1]*0.1))
        sel2=which(pentes==min(pentes))+1
        
        ref.table[2,]=c(F.i[sel2],pds.catch1[sel2], ssb.ts1[sel2],pds.stock.ts1[sel2], l.moy1[sel2],p.moy1[sel2],age.moy1[sel2])
        
        ### FMSP
        sel4=which(abs(msp.ts1-f.MSP*100)==min(abs(msp.ts1-f.MSP*100)))
        
        ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb.ts1[sel4],pds.stock.ts1[sel4], l.moy1[sel4],p.moy1[sel4],age.moy1[sel4])
        
    }else{
        
        ##############################################################################
        ##                          Tableau de YPR vs F.i                           ##
        ##############################################################################
        
        YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1,stock.w=pds.stock1, 
                ssn=ssn1,ssb=ssb1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
        
        
        
#        ##############################################################################
#        ##                       Tableau de YPR vs F.i Resume                       ##
#        ##############################################################################
#        
#        F.i2=seq(0,F.max, by=0.01)
#        F.i2=as.integer( F.i2*1000000)
#        F.i2=F.i2/1000000
#        
#        sel.Fi=which(YPR.table$F %in% F.i2)
#        YPR.table.short=YPR.table[sel.Fi,]
        
        
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
    }
    
    r.names=c("F.zero","F.01","F.max",f.MSP.name)
    row.names(ref.table)=r.names
    
    ref.line.sel=data.frame(line.sel=c(sel2,sel4, sel3))
    rownames(ref.line.sel)=r.names[c(2,4,3)]
    
    res=new("ypr",
            parms=parms,
            base=YPR,
            refs=ref.table,
            YPR=YPR.table)
    
    class(parms)
    res
    
}


full.sel <- function(sel.full, L) {
    full=which(L >=sel.full)
    mat.sel=L*0
    mat.sel[full]=1
    mat.sel=as.integer(mat.sel)
    return(mat.sel)
}
ramp.sel <- function(sel.zero, sel.full, L) {
    mod.ramp=coef(lm(c(0,1)~c(sel.zero,sel.full)))
    ramp=which(L >= sel.zero & L <= sel.full)
    zero=which(L < sel.zero)
    full=which(L >sel.full)
    
    F.sel=vector()
    F.sel[zero]=0
    F.sel[ramp]=L[ramp]*mod.ramp[2]+mod.ramp[1]
    F.sel[full]=1
    return(F.sel)
}
logistic.sel <- function(alpha,beta, L) {
    Fsel=1/(1+exp(-(alpha+beta*(L))))
    return(F.sel)
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
            if(object@parms$riv.calc){
                title= "Length based Yield per Recruit\n   Rivard weights calculations"                
            }else{
                title= "Length based Yield per Recruit"                
            }
            
            # Title:
            cat("\nTitle:\n ",title, "\n", sep = "")
            
            #VonB parameters:
            cat("\nvon Bartalanffy growth parameters:\n Linf=", coef(object@parms$vonB)[[1]], "  K=",coef(object@parms$vonB)[[2]] , sep = "")
            
            #LW parameters:
            cat("\nLength-Weight curve parameters:\n log(alpha)=", log(coef(object@parms$LW)[[2]]), "  beta=",coef(object@parms$LW)[[1]] , sep = "")
            
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
    ylim1=c(0,max(YPR$ypr)*1.1)
    ylim2=c(0,max(YPR$ssb)*1.1)
    plot(YPR$ypr~YPR$F  ,main=main,ylim=ylim1, 
            ylab=ylab.ypr,xlab=xlab,type='l', lwd=3, col=col.ypr, las=1)
    if(ref){
        for(i in 2:dim(refs)[1]){
            lines(c(-1,YPR)~c(F,F),data=refs[i,], lty=2)
        }
        points(YPR~F,data=refs[2:dim(refs)[1],], pch=21, col='black', bg='white',cex=1.2)
        y.coord=par('usr')[2]*0.01
        r.names=rownames(refs)
        
        text(x=refs[2,1], y=y.coord, labels=r.names[2], srt=90,adj=c(0.2,1.2) , cex=0.8, font=2)
        text(x=refs[3,1], y=y.coord, labels=r.names[3], srt=90,adj=c(0.13,1.2), cex=0.8, font=2)
        text(x=refs[4,1], y=y.coord, labels=r.names[4], srt=90,adj=c(0.2,-0.4) , cex=0.8, font=2)
    }
    
    par(new=TRUE)
    plot(YPR$ssb~YPR$F,type='l',xaxt="n",yaxt="n",xlab="",ylab="", lwd=3, col=col.ssb, ylim=ylim2)
    
    if(ref)points(SSB.R~F,data=refs[2:dim(refs)[1],], pch=21, col='black', bg='white',cex=1.2)
    
    axis(4, las=1)
    mtext(ylab.ssb,side=4,line=2.9)
    if(legend){
        legend("topright",col=c("blue","red"),lty=1, lwd=3,legend=c("YPR","SSB/R"),
                horiz=TRUE, bty='n', bg='white', seg.len=1)
    }
}


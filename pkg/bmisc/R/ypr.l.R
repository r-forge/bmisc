ypr.l <- 
        function(fsel.type,last.age, l.start, age.step=1, LW, vonB, F.max=2, 
                F.incr.YPR=0.0001, M=0.2, mat, f.MSP=0.4, riv.calc=TRUE, F.part=0, M.part=0.5) {
  
    age=seq(0,last.age,by=age.step)
    age=as.integer(age*1000000)
    age=age/1000000
    
    l.age=vonB[1]*(1-exp(-vonB[2]*age)) + l.start*exp(-vonB[2]*age)

    if(riv.calc){
        p.age=LW[1]*l.age^LW[2]
        p.age.riv=rivard(data.frame(p.age,p.age),pred=FALSE,plus.gr=FALSE)[,2]
        YPR=data.frame(age,l.age, p.age, p.age.riv)
    }else{
        p.age=LW[1]*l.age^LW[2]
        YPR=data.frame(age,l.age, p.age)
    }
    
    F.i=seq(0,F.max, by=F.incr.YPR)
    F.i=as.integer( F.i*1000000)
    F.i=F.i/1000000
    n.F=n(F.i)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
    switch(mat[[1]],
            full= mat.sel <- full.sel(sel.full=mat[[2]], L = YPR$l.age),
            ramp= mat.sel <- ramp.sel(sel.zero =mat[[2]], sel.full = mat[[3]], L = YPR$l.age),
            logistic= mat.sel <- logistic.sel(alpha=mat[[2]], beta=mat[[3]], L=YPR$l.age)
    )    
    
    ##############################################################################
    ##                             F selectivity                                ##
    ##############################################################################
    switch(fsel.type[[1]],
            ramp= F.sel <- ramp.sel(sel.zero =fsel.type[[2]], sel.full = fsel.type[[3]], L = YPR$l.age),
            logistic= F.sel <- logistic.sel(alpha=fsel.type[[2]], beta=fsel.type[[3]], L=YPR$l.age)
    )
    
    
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
        title= "Length based Yield per Recruit\n   Rivard weights calculations"

        F.ts=sweep(F.,MARGIN=2,F.part,`*`)
        M.ts=M*M.part
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
        
        title="Length based Yield per Recruit"
        
        ##############################################################################
        ##                          Tableau de YPR vs F.i                           ##
        ##############################################################################
        
        YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1,stock.w=pds.stock1, 
                ssn=ssn1,ssb=ssb1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
        
        
        
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
    
    ref.line.sel=data.frame(c(sel2,sel4, sel3))
    rownames(ref.line.sel)=r.names[c(4,2,3)]
    
    res=new("ypr",base=YPR,
            ref=ref.table,
            YPR.short=YPR.table.short,
            YPR=YPR.table,
            ref.line.sel=ref.line.sel,
            VonB.parms=vonB,
            LW.parms=LW,
            title=title)
    
    
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
                base="data.frame",
                ref="data.frame",
                YPR.short="data.frame",
                YPR="data.frame",
                ref.line.sel="data.frame",
                title = "character",
                VonB.parms="numeric",
                LW.parms="numeric")
)


setMethod("show", "ypr",
        function(object){
            ref.names=rownames(object@ref)
            dat=object@ref[,1]
            names(dat)=ref.names
            print(dat)
        }
)


summary.ypr=function(object){
    
    # Title:
    cat("\nTitle:\n ",object@title, "\n", sep = "")
    
    #VonB parameters:
    cat("\nvon Bartalanffy growth parameters:\n Linf=", object@VonB.parms[[1]], "  K=",object@VonB.parms[[2]] , sep = "")
    
    #LW parameters:
    cat("\nLength-Weight curve parameters:\n log(alpha)=", log(object@LW.parms[[1]]), "  beta=",object@LW.parms[[2]] , sep = "")
    
    # Test Results:
    results = object@ref
    cat("\n\nResults:\n", sep = "")
    print(results)
}


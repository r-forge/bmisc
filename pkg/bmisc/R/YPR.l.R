YPR.l <- 
        function(fsel.type,last.age=14, l.start, age.step=1, LW, vonB, F.max=0.2, 
                F.incr.YPR=0.1, M=0.2, full.mat, f.MSP=0.4, riv.calc=FALSE) {
    
    t=seq(0,last.age,by=age.step)
    t=as.integer(t*1000000)
    t=t/1000000

    l.age=vonB[1]*(1-exp(-vonB[2]*t)) + l.start*exp(-vonB[2]*t)
    if(riv.calc){
        p.age1=LW[1]*l.age^LW[2]
        p.age=rivard(data.frame(p.age1,p.age1),pred=FALSE,plus.gr=FALSE)[,2]
    }else{p.age=LW[1]*l.age^LW[2]}
    
    YPR=data.frame(t,l.age, p.age)
    
    
        
    switch(fsel.type[[1]],
            ramp= F.sel <- fish.select.ramp(sel.zero =fsel.type[[2]], sel.full = fsel.type[[3]], L = YPR$l.age),
            logistic= F.sel <- fish.select.logistic(alpha=fsel.type[[2]], beta=fsel.type[[3]], L=YPR$l.age)
    )
    
    F.i=seq(0,F.max, by=F.incr.YPR)
    F.i=as.integer( F.i*1000000000)
    F.i=F.i/1000000000
    n.F=n(F.i)
    
    
    ##############################################################################
    ##                          Matrices des calculs                            ##
    ##############################################################################
    
    
    F.=matrix(rep(F.sel,n.F),ncol=n.F, nrow=n(F.sel))
    
    
    F.=sweep(F.,MARGIN=2,F.i,`*`)
    
    F.ts=sweep(F.,MARGIN=2,0.5,`*`)
    M.ts=M*0.5
    Z=F.+M
    Z.ts=F.ts+M.ts
    
    Z1=colSums(Z, na.rm=TRUE)
    
    
    n.stock=Z
    n.stock[1,]=1
    
    for(i in 1:(n(F.sel)-1)){
        n.stock[i+1,]=n.stock[i,]*exp(-Z[i,])
    }
    n.stock.ts=n.stock
    
    for(i in 1:(n(F.sel)-1)){
        n.stock.ts[i,]=n.stock[i,]*exp(-Z.ts[i,])
    }
    n.stock1=colSums(n.stock, na.rm=TRUE)
    n.stock.ts1=colSums(n.stock.ts, na.rm=TRUE)

    
    pds.stock=sweep(n.stock,MARGIN=1,YPR$p.age,FUN="*")
    pds.stock1=colSums(pds.stock, na.rm=TRUE)
    
    f.mat=YPR$l.age
    f.mat[f.mat<full.mat]=0
    f.mat[f.mat>full.mat]=1
    pds.stock.ts=sweep(n.stock.ts,MARGIN=1,YPR$p.age,FUN="*")
    
    pds.stock.ts=sweep(pds.stock.ts,MARGIN=1,f.mat,FUN="*")
    pds.stock.ts1=colSums(pds.stock.ts, na.rm=TRUE)
    
    pds.stock.moy=pds.stock1/n.stock1
    
    
    n.catch=F./(F.+M)* n.stock*(1-exp(-Z))
    n.catch[1,]=NA
    
    n.catch1=colSums(n.catch, na.rm=TRUE)
    
    pds.catch=n.stock
    
    pds.catch=sweep(n.catch,MARGIN=1,YPR$p.age,"*")
    pds.catch1=colSums(pds.catch, na.rm=TRUE)
    
    
    ssn.ts=n.stock.ts
    ssn.ts[YPR$l.age<full.mat,]=0
    ssn.ts1=colSums(ssn.ts, na.rm=TRUE)
    
    ssn=n.stock
    ssn[YPR$l.age<full.mat,]=0
    ssn1=colSums(ssn, na.rm=TRUE)
    
    ssb=sweep(ssn,MARGIN=1,YPR$p.age,"*")
    ssb1=colSums(ssb, na.rm=TRUE)
    
    
    msp1=ssb1/max(ssb1)*100
    
    
    l.moy=sweep(n.stock,MARGIN=1,YPR$l.age,"*")
    l.moy1=colSums(l.moy, na.rm=TRUE)/n.stock1
    
    p.moy=sweep(n.stock,MARGIN=1,YPR$p.age,"*")
    p.moy1=colSums(p.moy, na.rm=TRUE)/n.stock1
    
    age.moy=sweep(n.stock,MARGIN=1,YPR$t,"*")
    age.moy1=colSums(age.moy, na.rm=TRUE)/n.stock1
    
    
    
    ##############################################################################
    ##                          TRUEableau de YPR vs F.i                            ##
    ##############################################################################
    
    YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1,stock.w=pds.stock1, 
            ssn=ssn1,ssb=ssb1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
    
    
    ##############################################################################
    ##                     TRUEableau des points de references                  ##
    ##############################################################################
    f.MSP.name=paste('F',round(f.MSP*100,digits=0),sep=".")
    ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA)
    
    ### F0
    sel1=which(F.i==0)
    ref.table[1,]=c(F.i[sel1],pds.catch1[sel1], ssb1[sel1],pds.stock1[sel1])
    
    ### FMAX
    sel3=which(pds.catch1==max(pds.catch1))
    ref.table[3,]=c(F.i[sel3],pds.catch1[sel3], ssb1[sel3],pds.stock1[sel3])
    
    ###F01
    n.lm=length(pds.catch1)
    lm.mod=vector()
    for(i in 1:(n.lm-1)) lm.mod[i]=(pds.catch1[i+1]-pds.catch1[i])/(F.i[i+1]-F.i[i])
    
    pentes=abs(lm.mod-(lm.mod[1]*0.1))
    sel2=which(pentes==min(pentes))+1
    
    ref.table[2,]=c(F.i[sel2],pds.catch1[sel2], ssb1[sel2],pds.stock1[sel2])
    
    ### FMSP
    sel4=which(abs(msp1-f.MSP*100)==min(abs(msp1-f.MSP*100)))
#    sel4=c(2195,2196,2197)
    ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb1[sel4],pds.stock1[sel4])
    
    row.names(ref.table)=c("F.zero","F.01","F.max",f.MSP.name)
    
    res=list(ref=ref.table,ypr=YPR.table)
    
    print(res$ref)
    res
}

fish.select.ramp <- function(sel.zero, sel.full, L) {
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

fish.select.logistic <- function(alpha,beta, L) {
    Fsel=1/(1+exp(-(alpha+beta*(L))))
    return(F.sel)
}






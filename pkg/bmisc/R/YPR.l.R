ypr.l <- 
        function(fsel.type,last.age, l.start, age.step=1, LW, vonB, F.max=2, 
                F.incr.YPR=0.0001, M=0.2, mat, f.MSP=0.4, riv.calc=FALSE) {
    
    ti=seq(0,last.age,by=age.step)
    ti=as.integer(ti*1000000)
    ti=ti/1000000
    
    l.age=vonB[1]*(1-exp(-vonB[2]*ti)) + l.start*exp(-vonB[2]*ti)
    
    if(riv.calc){
        p.age=LW[1]*l.age^LW[2]
        p.age.riv=rivard(data.frame(p.age,p.age),pred=FALSE,plus.gr=FALSE)[,2]
        YPR=data.frame(ti,l.age, p.age, p.age.riv)
    }else{
        p.age=LW[1]*l.age^LW[2]
        YPR=data.frame(ti,l.age, p.age)
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
    
    
    ###
    ssn=sweep(n.stock,MARGIN=1,mat.sel,FUN="*")
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
    ##                          Tableau de YPR vs F.i                           ##
    ##############################################################################
    
    YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1,stock.w=pds.stock1, 
            ssn=ssn1,ssb=ssb1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
    
    
    ##############################################################################
    ##                     Tableau des points de references                     ##
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
    
    ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb1[sel4],pds.stock1[sel4])
    
    r.names=c("F.zero","F.01","F.max",f.MSP.name)
    row.names(ref.table)=r.names
    
    ref.line.sel=data.frame(c(sel2,sel4, sel3))
    rownames(ref.line.sel)=r.names[c(4,2,3)]
    res=list(ref=ref.table,ypr=YPR.table, ref.line.sel=ref.line.sel)
    
    print(res$ref)
    
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






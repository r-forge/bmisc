YPR.l <-
function(fsel.type,last.age, vonB, l.start, LW, F.max, F.incr.YPR, M, full.mat, f.MSP) {
    
    t=seq(last.age)
    l.age=vonB[1]*(1-exp(-vonB[2]*t)) + l.start*exp(-vonB[2]*t)
    p.age=LW[1]*l.age^LW[2]
    
    YPR=data.frame(t,l.age, p.age)
    
    #fsel.type[[1]]=match.arg(fsel.type[[1]], choices=c("rampn","logistic"))
    
    switch(fsel.type[[1]],
            ramp= F.sel <- .fish.select.ramp(sel.zero =fsel.type[[2]], sel.full = fsel.type[[3]], L = YPR$l.age),
            logistic= F.sel <- .fish.select.logistic(alpha=fsel.type[[2]], beta=fsel.type[[3]], L=YPR$l.age)
    )
    
    Fi=seq(0,F.max, by=F.incr.YPR)
    n.F=n(Fi)
    
    
    ##############################################################################
    ##                          Matrices des calculs                            ##
    ##############################################################################
    
    
    F=matrix(ncol=n.F, nrow=n(F.sel))
    for(i in Fi){
        j=which(Fi==i)
        F[,j]=F.sel*i
    }
    F1=colSums(F, na.rm=TRUE)
    
    
    Z=F+M
    Z1=colSums(Z, na.rm=TRUE)
    
    
    n.stock=Z
    n.stock[1,]=1
    for(i in 1:(n(F.sel)-1)){
        n.stock[i+1,]=n.stock[i,]*exp(-Z[i,])
    }
    n.stock1=colSums(n.stock, na.rm=TRUE)
    
    pds.stock=n.stock
    for(i in 1:n(Fi)){
        pds.stock[,i]=n.stock[,i]*YPR$p.age
    }
    pds.stock1=colSums(pds.stock, na.rm=TRUE)
    
    pds.stock.moy=pds.stock1/n.stock1
    
    n.catch=n.stock
    n.catch[1,]=NA
    for(i in 1:(n(F.sel))){
        n.catch[i,]=  (F[i,]/(F[i,]+M)) * n.stock[i,]*(1-exp(-Z[i,]))
    }
    n.catch1=colSums(n.catch, na.rm=TRUE)
    
    pds.catch=n.stock
    for(i in 1:n(Fi)){
        pds.catch[,i]=n.catch[,i]*YPR$p.age
    }
    pds.catch1=colSums(pds.catch, na.rm=TRUE)
    
    
    ssn=n.stock
    ssn[YPR$l.age<full.mat,]=0
    ssn1=colSums(ssn, na.rm=TRUE)
    
    ssb=ssn
    for(i in 1:n(Fi)){  
        ssb[,i]=  ssn[,i]*YPR$p.age
    }
    ssb1=colSums(ssb, na.rm=TRUE)
    
    
    msp1=ssb1/max(ssb1)*100
    
    l.moy=n.stock
    for(i in 1:n(Fi)){
        l.moy[,i]=n.stock[,i]*YPR$l.age
    }
    l.moy1=colSums(l.moy, na.rm=TRUE)/n.stock1
    
    p.moy=n.stock
    for(i in 1:n(Fi)){
        p.moy[,i]=n.stock[,i]*YPR$p.age
    }
    p.moy1=colSums(p.moy, na.rm=TRUE)/n.stock1
    
    age.moy=n.stock
    for(i in 1:n(Fi)){
        age.moy[,i]=n.stock[,i]*YPR$t
    }
    age.moy1=colSums(age.moy, na.rm=TRUE)/n.stock1
    
    
    
    ##############################################################################
    ##                          Tableau de YPR vs Fi                            ##
    ##############################################################################
    
    YPR.table=data.frame(F=Fi, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock1, 
            ssn=ssn1,f.MSP=msp1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
    
#labels=c("F","Catch Numbers","Yield per Recruit","Stock Numbers", "Stock Weight","SSB/R","% Maximum Spawning","Average Length", "Average Stock Weight","Average Age")
#
#attr(YPR.table,"label") <- labels[i]
#for(i in seq(dim(YPR.table)[2])){
#    attr(YPR.table[,i],"label") <- labels[i]
#}
    
    ##############################################################################
    ##                     Tableau des points de references                  ##
    ##############################################################################
    f.MSP.name=paste('F',round(f.MSP*100,digits=0),sep=".")
    ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA)
    
    sel1=which(Fi==0)
    ref.table[1,]=c(Fi[sel1],pds.catch1[sel1], ssb1[sel1],pds.stock1[sel1])
    
    sel3=which(pds.catch1==max(pds.catch1))
    ref.table[3,]=c(Fi[sel3],pds.catch1[sel3], ssb1[sel3],pds.stock1[sel3])
    
    sel2=which(abs((ref.table[3,2]*0.9)-pds.catch1)==min(abs((ref.table[3,2]*0.9)-pds.catch1)))
    ref.table[2,]=c(Fi[sel2],pds.catch1[sel2], ssb1[sel2],pds.stock1[sel2])
    
    sel4=which(abs(msp1-f.MSP*100)==min(abs(msp1-f.MSP*100)))
    ref.table[4,]=c(Fi[sel4],pds.catch1[sel4], ssb1[sel4],pds.stock1[sel4])
    
    row.names(ref.table)=c("F.zero","F.01","F.max",f.MSP.name)
    
    res=list(ref=ref.table,ypr=YPR.table)
    
    res
}


.fish.select.ramp <- function(sel.zero, sel.full, L) {
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

.fish.select.logistic <- function(alpha,beta, L) {
    Fsel=1/(1+exp(-(alpha+beta*(L))))
    return(F.sel)
}
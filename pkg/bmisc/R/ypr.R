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
        Fsel.type=NULL, F.max=2, F.incr.YPR=0.0001,Mat,  Msel.type=NULL, 
        M=0.2, f.MSP=0.4){ #, F.f=0, M.f=0.5, riv.calc=FALSE){
    
    parms=list(Fsel.type=Fsel.type,last.age=last.age, l.start=l.start, prop.surv=prop.surv, fish.lim=fish.lim,
            age.step=age.step, LW=LW, vonB=vonB, F.max=F.max, F.incr.YPR=F.incr.YPR,
            Msel.type=Msel.type, M=M, Mat=Mat, f.MSP=f.MSP)#, riv.calc=riv.calc, F.f=F.f, M.f=M.f)
    
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
    
    
#    if(riv.calc){
#        p.age.riv=rivard(data.frame(p.age,p.age),pred=FALSE,plus.gr=FALSE)[,2]
#        YPR=data.frame(age,l.age, p.age, p.age.riv)
#    }else{
        YPR=data.frame(age,l.age, p.age)
#    }
    
    F.i=seq(0,F.max, by=F.incr.YPR)
    F.i=as.integer( F.i*1000000)
    F.i=F.i/1000000
    n.F=n(F.i)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
    mat=selectivity(Mat)

    ##############################################################################
    ##                                  Msel.type                               ##
    ##############################################################################
	M.sel=selectivity(Msel.type)
    
    ##############################################################################
    ##                             F selectivity                                ##
    ##############################################################################
	F.sel=selectivity(Fsel.type)

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
    ##            recalcul en fonction du premier janvier avec Rivard           ##
    ##############################################################################
#	if(riv.calc){
#		
#		F.ts=sweep(F.,MARGIN=2,F.f,`*`)
#		M.ts=M*M.f
#		Z.ts=F.ts+M.ts
#		
#		n.stock.ts=n.stock*exp(-Z.ts)
#		
#		n.stock.ts1=colSums(n.stock.ts, na.rm=TRUE)
#		
#		pds.stock.ts=sweep(n.stock,MARGIN=1,YPR$p.age.riv,FUN="*")
#		pds.stock.ts1=colSums(pds.stock.ts, na.rm=TRUE)
#		pds.stock.ts1[201]
#		
#		W=n.stock.ts*pds.stock.ts
#		Wts=sweep(W,MARGIN=1,mat,FUN="*")
#		Wts1=colSums(Wts, na.rm=TRUE)
#		Wts1[1]-ssb1[1]
#		
#		
#		ssn.ts=sweep(n.stock.ts,MARGIN=1,mat,FUN="*")
#		ssn.ts1=colSums(ssn.ts, na.rm=TRUE)
#		ssn.ts1[1]
#		
#		ssb.ts=sweep(ssn.ts,MARGIN=1,YPR$p.age.riv,"*")
#		ssb.ts1=colSums(ssb.ts, na.rm=TRUE)
#		ssb.ts1[1]
#		
#		msp.ts1=ssb.ts1/max(ssb.ts1)*100
#		msp.ts1[201]
#		
#		##############################################################################
#		##                          Tableau de YPR vs F.i                           ##
#		##############################################################################
#		YPR.table=data.frame(F=F.i, catch.num=n.catch1, ypr=pds.catch1, stock.num=n.stock.ts1,stock.w=pds.stock.ts1, 
#				ssn=ssn.ts1,ssb=ssb.ts1,f.MSP=msp.ts1, avr.l=l.moy1, avr.w=p.moy1, avr.age=age.moy1)
#		
#		##############################################################################
#		##                       Tableau de YPR vs F.i Resume                       ##
#		##############################################################################
#		
#		F.i2=seq(0,F.max, by=0.01)
#		F.i2=as.integer( F.i2*1000000)
#		F.i2=F.i2/1000000
#		
#		sel.Fi=which(YPR.table$F %in% F.i2)
#		YPR.table.short=YPR.table[sel.Fi,]
#		
#		
#		##############################################################################
#		##                     Tableau des points de references                     ##
#		##############################################################################
#		f.MSP.name=paste('F',round(f.MSP*100,digits=0),sep=".")
#		ref.table=data.frame(F=NA,YPR=NA,SSB.R=NA,TBmass.R=NA, avr.L=NA, avr.wgt=NA, avr.age=NA)
#		
#		### F0
#		sel1=which(F.i==0)
#		ref.table[1,]=c(F.i[sel1],pds.catch1[sel1], ssb.ts1[sel1],pds.stock.ts1[sel1], l.moy1[sel1],p.moy1[sel1],age.moy1[sel1])
#		
#		### FMAX
#		sel3=which(pds.catch1==max(pds.catch1))
#		ref.table[3,]=c(F.i[sel3],pds.catch1[sel3], ssb.ts1[sel3],pds.stock.ts1[sel3], l.moy1[sel3],p.moy1[sel3],age.moy1[sel3])
#		
#		###F01
#		n.lm=length(pds.catch1)
#		lm.mod=vector()
#		for(i in 1:(n.lm-1)) lm.mod[i]=(pds.catch1[i+1]-pds.catch1[i])/(F.i[i+1]-F.i[i])
#		
#		pentes=abs(lm.mod-(lm.mod[1]*0.1))
#		sel2=which(pentes==min(pentes))+1
#		
#		ref.table[2,]=c(F.i[sel2],pds.catch1[sel2], ssb.ts1[sel2],pds.stock.ts1[sel2], l.moy1[sel2],p.moy1[sel2],age.moy1[sel2])
#		
#		### FMSP
#		sel4=which(abs(msp.ts1-f.MSP*100)==min(abs(msp.ts1-f.MSP*100)))
#		
#		ref.table[4,]=c(F.i[sel4],pds.catch1[sel4], ssb.ts1[sel4],pds.stock.ts1[sel4], l.moy1[sel4],p.moy1[sel4],age.moy1[sel4])
#		
#	}else{
    
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
#	}
    
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

const.sel <- function(x) {
    .sel=x*0 +1
    return(.sel)
}

full.sel <- function(x, infl1, neg=FALSE, lv=0, uv=1) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    if(!neg){
        s=which(x >=infl1)
        .sel=x*0+lv[1]
        .sel[s]=uv[1]
    }else{
        s=which(x <=infl1)
        .sel=x*0+lv[1]
        .sel[s]=uv[1]
    }
    return(.sel)
}

plat.full.sel <- function(x, infl1, infl2, neg=FALSE, lv=c(0,0), uv=c(1,1)) {
    n.uv=n(uv)
    test=vector()
    for(i in 1:n.uv) test[i]=any(lv>uv[i])
    if(any(test))stop("Values of 'lv' should all be smaller or equal to 'uv' values.")
    if(any(c(lv,uv)<0) | any(c(lv,uv)>1))stop("Values 'lv' and 'uv' should be in [0,1].")
    
    s1=which(x < infl1)
    s2=which(x > infl2)
    s3=which(x >= infl1 & x <= infl2)
    if(!neg){
        .sel=vector()
        .sel[s1]=lv[1]
        if(length(lv)==2){
            .sel[s2]=lv[2]
        }else{.sel[s2]=lv[1]}
        .sel[s3]=uv[1]
    }else{
        .sel=vector()
        .sel[s3]=lv[1]
        .sel[s1]=uv[1]
        if(length(uv)>=2){
            .sel[s2]=uv[2]
        }else{.sel[s2]=uv[1] }
    }
    return(.sel)
}


ramp.sel <- function(x, infl1, infl2, neg=FALSE, lv=0, uv=1) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    ramp=which(x >= infl1 & x <= infl2)
    s1=which(x < infl1)
    s2=which(x >infl2)
    if(!neg){
        mod.ramp=coef(lm(c(lv[1],uv[1])~c(infl1,infl2)))
        .sel=vector()
        .sel[s1]=lv[1]
        .sel[ramp]=x[ramp]*mod.ramp[2]+mod.ramp[1]
        .sel[s2]=uv[1]
    }else{  mod.ramp=coef(lm(c(uv[1],lv[1])~c(infl1,infl2)))
        .sel=vector()
        .sel[s1]=uv[1]
        .sel[ramp]=x[ramp]* mod.ramp[2]+mod.ramp[1]
        .sel[s2]=lv[1]
    }
    return(.sel)
}

plat.ramp.sel <- function(x, infl1, infl2, infl3,infl4, neg=FALSE, lv=c(0,0), uv=c(1,1)) {
    n.uv=n(uv)
    test=vector()
    for(i in 1:n.uv) test[i]=any(lv>uv[i])
    if(any(test))stop("Values of 'lv' should all be smaller or equal to 'uv' values.")
    if(any(c(lv,uv)<0) | any(c(lv,uv)>1))stop("Values 'lv' and 'uv' should be in [0,1].")
    
    ramp=which(x >= infl1 & x <= infl2)
    ramp2=which(x >= infl3 & x <= infl4)
    s1=which(x < infl1 )
    s11=which(x > infl4)
    s2=which(x >infl2 & x <infl3)
    
    if(!neg){
        mod.ramp=coef(lm(c(lv[1],uv[1])~c(infl1,infl2)))
        .sel=vector()
        .sel[s1]=lv[1]
        .sel[s2]=uv[1]
        
        if(length(lv) >= 2){
            mod.ramp2=coef(lm(c(uv[1],lv[2])~c(infl3,infl4)))
            .sel[s11]=lv[2]
        }else{
            mod.ramp2=coef(lm(c(uv[1],lv[1])~c(infl3,infl4)))
            .sel[s11]=lv[1]
        }		
        .sel[ramp]=x[ramp]*mod.ramp[2]+mod.ramp[1]
        .sel[ramp2]=x[ramp2]*mod.ramp2[2]+mod.ramp2[1]
    }else{  
        mod.ramp=coef(lm(c(uv[1],lv[1])~c(infl1,infl2)))
        
        .sel=vector()
        .sel[s1]=uv[1]
        .sel[s2]=lv[1]
        if(length(uv)>=2){
            mod.ramp2=coef(lm(c(lv[1], uv[2])~c(infl3,infl4)))
            .sel[s11]=uv[2]
        }else{
            mod.ramp2=coef(lm(c(lv[1], uv[1])~c(infl3,infl4)))
            .sel[s11]=uv[1]}
        .sel[ramp]=x[ramp]*mod.ramp[2]+mod.ramp[1]
        .sel[ramp2]=x[ramp2]*mod.ramp2[2]+mod.ramp2[1]
    }
    return(.sel)
}


logit.sel <- function(x, infl1,infl2, neg=FALSE, lv=0, uv=1,...) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    
    res=find.beta(minv=infl1,maxv=infl2,...)
    ajust=coef(lm(c(uv,lv)~c(1,0)))
    if(!neg){
        .sel=(1/(1+exp(-res$beta*(x-res$x50))))
    }else{
        .sel=1/(1+exp(res$beta*(x-res$x50)))
    }
    .sel=.sel*ajust[2]+ajust[1]
    return(.sel)
}

plat.logit.sel <- function(x, infl1,infl2,infl3,infl4, neg=FALSE, lv=c(0,0), uv=c(1,1),...) {
    n.uv=n(uv)
    test=vector()
    for(i in 1:n.uv) test[i]=any(lv>uv[i])
    if(any(test))stop("Values of 'lv' should all be smaller or equal to 'uv' values.")
    if(any(c(lv,uv)<0) | any(c(lv,uv)>1))stop("Values 'lv' and 'uv' should be in [0,1].")
    
    s1=which(x < ((infl3-infl2)/2)+infl2)
    s2=which(x >= ((infl3-infl2)/2)+infl2)
    .sel=x
    
    if(!neg){
        ajust1=coef(lm(c(uv[1],lv[1])~c(1,0)))
        if(length(lv)>=2){
            ajust2=coef(lm(c(uv[1],lv[2])~c(1,0)))
        }else{ajust2=coef(lm(c(uv[1],lv[1])~c(1,0)))}
        
        res1=find.beta(minv=infl1,maxv=infl2,...)
        res2=find.beta(minv=infl3,maxv=infl4,...)
        .sel[s1]=1/(1+exp(-res1$beta*(x[s1]-res1$x50)))
        .sel[s1]=.sel[s1]*ajust1[2]+ajust1[1]
        
        .sel[s2]=1/(1+exp(res2$beta*(x[s2]-res2$x50)))
        .sel[s2]=.sel[s2]*ajust2[2]+ajust2[1]
    }else{
        ajust1=coef(lm(c(uv[1],lv[1])~c(1,0)))
        if(length(uv)>=2){
            ajust2=coef(lm(c(uv[2],lv[1])~c(1,0)))
        }else{ajust2=coef(lm(c(uv[1],lv[1])~c(1,0)))}
        
        res1=find.beta(minv=infl1,maxv=infl2,...)
        res2=find.beta(minv=infl3,maxv=infl4,...)
        .sel[s1]=1/(1+exp(res1$beta*(x[s1]-res1$x50)))
        .sel[s1]=.sel[s1]*ajust1[2]+ajust1[1]
        
        .sel[s2]=1/(1+exp(-res2$beta*(x[s2]-res2$x50)))
        .sel[s2]=.sel[s2]*ajust2[2]+ajust2[1]
    }	
    return(.sel)
}

mod.logit.sel <- function(x, alpha,beta) {
    .sel=1/(1+exp(-(alpha+beta*(x))))
    return(.sel)
}


selectivity <- function(sel.type, out){
	
	if(is.null(sel.type)){ .sel <- const.sel( x = YPR$l.age)
	}else{
		if(!(class(sel.type)[1] %in% c("list", "glm"))) stop("sel.type should either be a list or a glm (logit) object.")
	}
	if(class(sel.type)[1]=="list"){		
		switch(sel.type[[1]],
				const=  .sel <- const.sel( x = YPR$l.age),
				full= {
					if(all(!("infl1" %in% names(sel.type)))) stop("'infl1' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, neg=FALSE, lv=0, uv=1)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]		
					.sel=do.call("full.sel", sel.type1)
				},
				plat.full= {
					if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, infl2=NULL, neg=FALSE, lv=0, uv=1)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]		
					.sel=do.call("plat.full.sel", sel.type1)					
				},
				ramp= {
					if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, infl2=NULL, neg=FALSE, lv=0, uv=1)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]	
					.sel=do.call("ramp.sel", sel.type1)
				},
				plat.ramp= {
					if(all(!(c("infl1","infl2", "infl3", "infl4") %in% names(sel.type)))) stop("'infl1' to 'infl4' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, infl2=NULL, infl3=NULL, infl4=NULL, neg=FALSE, lv=0, uv=1)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]		
					.sel=do.call("plat.ramp.sel", sel.type1)
				},
				logit= {
					if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, infl2=NULL, neg=FALSE, lv=0, uv=1 , 
							prob=NULL, prop=0.1,beta=0.2, fast=TRUE)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]		
					.sel=do.call("logit.sel", sel.type1)
				},
				plat.logit= {
					if(all(!(c("infl1","infl2", "infl3", "infl4") %in% names(sel.type)))) stop("'infl1' to 'infl4' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, infl1=NULL, infl2=NULL, infl3=NULL, infl4=NULL, neg=FALSE, lv=0, uv=1, 
							prob=NULL, prop=0.1,beta=0.2, fast=TRUE)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]		
					.sel=do.call("plat.logit.sel", sel.type1)
				},
				mod.logit= {
					if(all(!(c("alpha","beta") %in% names(sel.type)))) stop("'alpha' and 'beta' must be defined. Read help('ypr').")
					sel.type1=list(x=YPR$l.age, alpha=NULL, beta=NULL)
					sel=which(names(sel.type1) %in% names(sel.type))
					name.sel=names(sel.type1)[sel]
					sel.type1[name.sel] <-sel.type[name.sel]	
					.sel=do.call("mod.logit.sel", sel.type1)
					
				}
		)   
	}
	
	if(class(sel.type)[1]=="glm"){
		coeffs=coef(sel.type)
		.sel <- mod.logit.sel(alpha=coeffs[[1]], beta=coeffs[[2]], x=YPR$l.age)
	}   
	return(.sel)
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
#            if(object@parms$riv.calc){
#                title= "Length based Yield per Recruit\n   Rivard weights calculations"                
#            }else{
                title= "Length based Yield per Recruit"                
#            }
            
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

plot.parms <- function (object) UseMethod("plot.parms")
plot.parms.ypr <- function(object){
    
    
    xlim=c(min(object@base$l.age,na.rm=T),max(object@base$l.age,na.rm=T))
    x.dat=seq(xlim[1],xlim[2], by=(xlim[2]-xlim[1])/1000)
    ylim=c(0,1)
    
    ##############################################################################
    ##                               Maturity                                   ##
    ##############################################################################
	mat=selectivity(object@parms$Mat)
	
    ##############################################################################
    ##                                  Msel.type                               ##
    ##############################################################################
	M.sel=selectivity(object@parms$Msel.type)
    
    ##############################################################################
    ##                             F selectivity                                ##
    ##############################################################################
	F.sel=selectivity(object@parms$Fsel.type)
   
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
    plot(mat~x.dat, xlim=xlim, ylim=ylim, main="Maturity\n(Mat)", type='l', lwd=2.7, xlab="Length", ylab="Probability")	
    plot(M.sel~x.dat  , xlim=xlim, ylim=ylim, main="Natural Mortality Selectivity\n(Msel.type)", type='l', lwd=2.7, xlab="Length", ylab="Probability")
    plot(F.sel~x.dat  , xlim=xlim, ylim=ylim, main="Fishing Selectivity\n(Fsel.type)", type='l', lwd=2.7, xlab="Length", ylab="Probability")
    if(!is.null(object@parms$prop.surv)){
        points(F.sel2~x.dat, type='l', lwd=2.5, col='blue' )
        abline(v=object@parms$fish.lim, lwd=2.5, col="red")
    }
}



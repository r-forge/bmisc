#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##     Selectivity functions for ypr()                                         ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-07-15                                                            ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################


.selectivity <- function(sel.type, x){
    NAME <- paste(deparse(substitute(sel.type), 500), collapse="\n")
    cl.sel=class(sel.type)[1]
    if(cl.sel=="function")cl.sel="fct"
    if(!(cl.sel %in% c("NULL","fct", "list", "glm"))) stop(paste(NAME," should be an object of class c('NULL', 'function', 'list','glm'). Read help('ypr')."))
    if(cl.sel=="list" & !(sel.type[[1]] %in% c("const","full","plat.full","ramp","plat.ramp","logit","plat.logit","mod.logit"))){
        stop(paste(NAME,"is a list. The fist value of this list should be one of\n  c('const','full','plat.full','ramp','plat.ramp','logit','plat.logit','mod.logit').\n\nRead help('ypr')."))
    }
    switch(cl.sel,
            fct = .sel<-sel.type(x),
            NULL=     .sel <- const.sel(x),
            glm={
                coeffs=coef(sel.type)
                .sel <- mod.logit.sel(alpha=coeffs[[1]], beta=coeffs[[2]], x=x)},
            list=               
                    switch(sel.type[[1]],
                            const=  .sel <- const.sel( x = x),
                            full= {
                                if(all(!("infl1" %in% names(sel.type)))) stop("'infl1' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, pos=TRUE, lv=0, uv=1)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]        
                                .sel=do.call("full.sel", sel.type1)},
                            plat.full= {
                                if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, infl2=NULL, pos=TRUE, lv=0, uv=1)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]        
                                .sel=do.call("plat.full.sel", sel.type1)                    },
                            ramp= {
                                if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, infl2=NULL, pos=TRUE, lv=0, uv=1)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]    
                                .sel=do.call("ramp.sel", sel.type1)},
                            plat.ramp= {
                                if(all(!(c("infl1","infl2", "infl3", "infl4") %in% names(sel.type)))) stop("'infl1' to 'infl4' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, infl2=NULL, infl3=NULL, infl4=NULL, pos=TRUE, lv=0, uv=1)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]        
                                .sel=do.call("plat.ramp.sel", sel.type1)},
                            logit= {
                                if(all(!(c("infl1","infl2") %in% names(sel.type)))) stop("'infl1' and 'infl2' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, infl2=NULL, pos=TRUE, lv=0, uv=1 , 
                                        prob=NULL, prop=0.1,beta=0.2, fast=TRUE)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]        
                                .sel=do.call("logit.sel", sel.type1)},
                            plat.logit= {
                                if(all(!(c("infl1","infl2", "infl3", "infl4") %in% names(sel.type)))) stop("'infl1' to 'infl4' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, infl1=NULL, infl2=NULL, infl3=NULL, infl4=NULL, pos=TRUE, lv=0, uv=1, 
                                        prob=NULL, prop=0.1,beta=0.2, fast=TRUE)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]        
                                .sel=do.call("plat.logit.sel", sel.type1)},
                            mod.logit= {
                                if(all(!(c("alpha","beta") %in% names(sel.type)))) stop("'alpha' and 'beta' must be defined. Read help('ypr').")
                                sel.type1=list(x=x, alpha=NULL, beta=NULL)
                                sel=which(names(sel.type1) %in% names(sel.type))
                                name.sel=names(sel.type1)[sel]
                                sel.type1[name.sel] <-sel.type[name.sel]    
                                .sel=do.call("mod.logit.sel", sel.type1)}
                    )
    
    )
    
    return(.sel)
}


const.sel <- function(x) {
    .sel=x*0 +1
    return(.sel)
}

full.sel <- function(x, infl1, pos=TRUE, lv=0, uv=1) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    if(pos){
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

plat.full.sel <- function(x, infl1, infl2, pos=TRUE, lv=c(0,0), uv=c(1,1)) {
    n.uv=n(uv)
    test=vector()
    for(i in 1:n.uv) test[i]=any(lv>uv[i])
    if(any(test))stop("Values of 'lv' should all be smaller or equal to 'uv' values.")
    if(any(c(lv,uv)<0) | any(c(lv,uv)>1))stop("Values 'lv' and 'uv' should be in [0,1].")
    
    s1=which(x < infl1)
    s2=which(x > infl2)
    s3=which(x >= infl1 & x <= infl2)
    if(pos){
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


ramp.sel <- function(x, infl1, infl2, pos=TRUE, lv=0, uv=1) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    ramp=which(x >= infl1 & x <= infl2)
    s1=which(x < infl1)
    s2=which(x >infl2)
    if(pos){
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

plat.ramp.sel <- function(x, infl1, infl2, infl3,infl4, pos=TRUE, lv=c(0,0), uv=c(1,1)) {
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
    
    if(pos){
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


logit.sel <- function(x, infl1,infl2, pos=TRUE, lv=0, uv=1,...) {
    if(lv[1]>uv[1]){stop("'lv' should be smaller or equal to 'uv'.")}
    if(any(c(lv[1],uv[1])<0) | any(c(lv[1],uv[1])>1)){stop("Values 'lv' and 'uv' should be in [0,1].")}
    
    res=find.beta(minv=infl1,maxv=infl2,...)
    ajust=coef(lm(c(uv[1],lv[1])~c(1,0)))
    if(pos){
        .sel=(1/(1+exp(-res$beta*(x-res$x50))))
    }else{
        .sel=1/(1+exp(res$beta*(x-res$x50)))
    }
    .sel=.sel*ajust[2]+ajust[1]
    return(.sel)
}

plat.logit.sel <- function(x, infl1,infl2,infl3,infl4, pos=TRUE, lv=c(0,0), uv=c(1,1),...) {
    n.uv=n(uv)
    test=vector()
    for(i in 1:n.uv) test[i]=any(lv>uv[i])
    if(any(test))stop("Values of 'lv' should all be smaller or equal to 'uv' values.")
    if(any(c(lv,uv)<0) | any(c(lv,uv)>1))stop("Values 'lv' and 'uv' should be in [0,1].")
    
    s1=which(x < ((infl3-infl2)/2)+infl2)
    s2=which(x >= ((infl3-infl2)/2)+infl2)
    .sel=x
    
    if(pos){
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




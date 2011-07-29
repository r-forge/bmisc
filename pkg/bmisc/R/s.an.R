###############################################################################
#                                                                             #
#                                                                             #
# Author: bruneaub                                                            #
###############################################################################

s.an=function(object,mat=c(800,1100,50), fisher.sel=NA ,fsel=NA, fsel.min=NA, M=NA,last.age=NA, f.MSP=NA, lstart=NA){
        
    parms=list(mat=mat, fsel=fsel, fsel.min=fsel.min, M=M,last.age=last.age, f.MSP=f.MSP, lstart=lstart)
    if(class(object)!="ypr") stop("'object' must be of class 'ypr' resulting from ypr.l() function. Read help('ypr.l').")
    if(all(is.na(parms)))
        stop("\nSensitivity analysis can not be done without\n specifying which parameter to test.\n")
    multi.sel=which(!is.na(parms))
    if(n(multi.sel)>1){
        warning(paste("Multiple parameters have been define for testing.\n This can not be done for now.\n The parameter '",
                        names(multi.sel)[1],"' will be used for the analysis.\n", sep=""))
        parm=parms[multi.sel[1]]
    }else{parm=parms[multi.sel]}
    
    if(names(parm)=='mat') {
        
        cat(paste("The '", names(parm),"' parameter in the ypr object is of type '",
                        object@parms$mat[[1]],"'. Read help('ypr.i')\n", sep=""))
        if(object@parms$mat[[1]]!='full')stop("Sensitivity anlysis can only be done for maturity of type 'full' for now.")
        parm.vals=parm[[1]]
        parm.seq=seq(parm.vals[1],parm.vals[2],by=parm.vals[3])
        parms.seq.n=seq(n(parm.seq))
        
        refs=list(NULL)
        ref.line.sels=list(NULL)
        for(i in parms.seq.n){
            k=parm.seq[i]
            name=paste("ypr.mat",k, sep=".")
            assign(name,ypr.l(fsel.type=object@parms$fsel.type, vonB=object@parms$vonB,  LW=object@parms$LW,
                            l.start=object@parms$l.start,last.age=object@parms$last.age,age.step=object@parms$age.step,
                            F.max=object@parms$F.max, F.incr.YPR=object@parms$F.incr.YPR, M=object@parms$M,
                            f.MSP=object@parms$f.MSP, riv.calc=object@parms$riv.calc, F.f=object@parms$F.f, M.f=object@parms$M.f,
                            mat=list(object@parms$mat[[1]],k))
            
            )
            refs[[i]]=get(name)@ref
            names(refs)[[i]]=name
            #ref.line.sels[[i]]=get(name)@ref.line.sel
            #names(ref.line.sels)[[i]]=name
            cat(paste(n(parm.seq)-i,"left\n"))
        }
    }
    
#    if(names(parm)=='fsel') {
#        cat(paste("The '", names(parm),"' parameter in the ypr object is of type '",
#                        object@parms$mat[[1]],"'. Read help('ypr.i')\n", sep=""))
#        
#        parm.vals=parm[[1]]
#        parm.seq=seq(parm.vals[1],parm.vals[2],by=parm.vals[3])
#        parms.seq.n=seq(n(parm.seq))
#        
#        diff.parm=parm.seq-object@parms$fsel.type[3]
#        parm.seq2=object@parms$fsel.type[3]+diff.parm
#        
#        parms.seq=data.frame(parm.seq,parm.seq2)
    ############################################################    
#        refs=list(NULL)
#        ref.line.sels=list(NULL)
#        for(i in parms.seq.n){
#            k=parm.seq[i]
#            name=paste("ypr.mat",k, sep=".")
#            assign(name,ypr.l(fsel.type=object@parms$fsel.type, vonB=object@parms$vonB,  LW=object@parms$LW,
#                            l.start=object@parms$l.start,last.age=object@parms$last.age,age.step=object@parms$age.step,
#                            F.max=object@parms$F.max, F.incr.YPR=object@parms$F.incr.YPR, M=object@parms$M,
#                            f.MSP=object@parms$f.MSP, riv.calc=object@parms$riv.calc, F.f=object@parms$F.f, M.f=object@parms$M.f,
#                            mat=list(object@parms$mat[[1]],k))
#            
#            )
#            refs[[i]]=get(name)@ref
#            names(refs)[[i]]=name
#            #ref.line.sels[[i]]=get(name)@ref.line.sel
#            #names(ref.line.sels)[[i]]=name
#            cat(paste(n(parm.seq)-i,"left\n"))
            
    title=paste(object@title,"\n   sensitivity analysis for '", names(parm),"' in ",
            parm.vals[1]," to ",parm.vals[2], " by ",parm.vals[3],sep="")
    res=new("ypr.sens",
            parms=object@parms,
            refs=refs,
            title=title)
    
    
    res
}

setClass("ypr.sens",
        representation(
                parms="list",
                refs="list",
                title = "character"),
)

setMethod("show", "ypr.sens",
        function(object){
            num.refs=n(object@refs)
            F.moy=object@refs[[1]][,1:2]
            YPR.moy=object@refs[[1]][,1:2]
            SSB.moy=object@refs[[1]][,1:2]
            for(i in seq(num.refs)){
                F.moy[,i]=object@refs[[i]]$F
                YPR.moy[,i]=object@refs[[i]]$YPR
                SSB.moy[,i]=object@refs[[i]]$SSB
            }
            res=object@refs[[1]][,1:3]
            res[,1]=rowMeans(F.moy)
            res[,2]=rowMeans(YPR.moy)
            res[,3]=rowMeans(SSB.moy)
            names(res)=paste(names(res),"moy", sep=".")
            print(res)
        }
)

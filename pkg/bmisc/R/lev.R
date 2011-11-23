lev <- function (y,data,...) {
    if( !missing(data)) {attach(data)}
    UseMethod("lev")
    #if(!is.null(data)) {detach(data)}
    
}


lev.default <-  function (y,  group, data,  trim.alpha = 0.1, type="abs", form,source="default", ...)
{
    test=deparse(substitute(group))
    test=substr(test[1],1,1)
    
    if (!is.numeric(y))
        stop(deparse(substitute(y)), " is not a numeric variable")
    call=match.call()
    
    if(test=="s" | source=="form"){
                test=NULL
        }else{
                vars1=as.vector(strsplit(deparse(substitute(group)), ", ")[[1]])
                nvars=length(vars1)
                if(test=="p"){
                        vars1[1]=substr(vars1[1],7,nchar(vars1[1]))
                }else{
                        if(test=="i"){
                                vars1[1]=substr(vars1[1],13,nchar(vars1[1]))
                        }else{
                                stop("Use 'paste' or 'interaction' for 'group'. See ?mc.long for examples on how to define 'group'")
                        }
                }
                vars1[nvars]=substr(vars1[nvars],1,nchar(vars1[nvars])-1)
                vars=vars1[1]
                for (i in 2:nvars) vars=paste(vars,"*",vars1[i],sep="")
                    
                

        }
    if(missing(form)){
     form =formula(paste(deparse(substitute(y)), "~",vars))
     environment(form)=.GlobalEnv
    }


    if ( trim.alpha >= 0.5) {
        stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean option")
    }

    test=NULL
    test=deparse(substitute(group))
    test=substr(test[1],1,1)
    if(!is.null(test) & length(test)==0 ){
        stop(deparse(substitute(group)), " should be the concatenation of the factors levels. Use 'paste()' or 'interaction()'.")
    }

    group <- as.factor(group)

    means <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], mean)
    switch(type,
            abs = resp.mean <- abs(y - means[group]),
            sq  = resp.mean <- (y - means[group])^2)
    res.mean= y - means[group]
    statistic = Anova(lm(resp.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
    METHOD = "Classical Levene's test based on the absolute deviations from the mean."
    p.value = Anova(lm(resp.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
    STATISTIC = statistic
    names(STATISTIC) = "F.value"
    Meval <- list(statistic = STATISTIC, p.value = p.value,
                    method = METHOD)


    meds <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], median)
    switch(type,
            abs = resp.med <- abs(y - meds[group]),
            sq  = resp.med <- (y - meds[group])^2)
    res.med= y - meds[group]
    statistic = Anova(lm(resp.med ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
    METHOD = "Modified Robust Brown-Forsythe Levene-type test based on the absolute deviations from the median. For non-normal uncentered leptokurtic residuals."
    p.value = Anova(lm(resp.med ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
    STATISTIC = statistic
    names(STATISTIC) = "F.value"
    Medval <- list(statistic = STATISTIC, p.value = p.value,
                    method = METHOD)


    trimmed.mean <- function(y) mean(y, trim = trim.alpha)
    trim.means <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], trimmed.mean)
    switch(type,
            abs = resp.trim.mean <- abs(y - trim.means[group]),
            sq  = resp.trim.mean <- (y - trim.means[group])^2)
    res.trim.mean=y - trim.means[group]

    statistic = Anova(lm(resp.trim.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
    METHOD = "Modified Robust Levene-type test based on the absolute deviations from the trimmed mean. It eliminates outliers."
    p.value = Anova(lm(resp.trim.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
    STATISTIC = statistic
    names(STATISTIC) = "F.value"
    Tmeanval <- list(statistic = STATISTIC, p.value = p.value,
                    method = METHOD)


    grouppn <- tapply(y,group,n)
    meds <- tapply(y, group, median)
    num=ifelse(grouppn[group]-2 > 0, ( (grouppn[group]-1.5)*grouppn[group]*((y-means[group])**2) - 0.5*tapply((y-means[group])**2,group,sum)[group] )    ,NA)
    den= (grouppn[group]-1)*(grouppn[group]-2)
    r=num/den
    statistic = Anova(lm(r ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
    METHOD = "Modified Robust O'Brien Levene-type test based on scores. For non-normal centered meso or platykurtic residuals."
    p.value = Anova(lm(r ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
    STATISTIC = statistic
    names(STATISTIC) = "F.value"
    Oval <- list(statistic = STATISTIC, p.value = p.value,
                    method = METHOD)



    try(if(!is.null(data)) {detach(data)} ,silent=TRUE)

    PVAL = c(Meval$p.value, Tmeanval$p.value, Medval$p.value,Oval$p.value)
    names(PVAL) = c(
            "Levene                  ",
            "Levene on trimed mean   ",
            "Brown.Forsythe          ",
            "OBrien                  ")

    STATISTIC = c(Meval$statistic, Tmeanval$statistic, Medval$statistic,Oval$statistic)
    names(STATISTIC) = c(
            "Levene                  ",
            "Levene on trimed mean   ",
            "Brown.Forsythe          ",
            "OBrien                  ")

    datall=data.frame(lev=res.mean, lev.tr=res.trim.mean, bf=res.med, ob=r, group=group)

    if(is.null(form)){
        RVAL = new('lev',
                formula=form,
                statistics = STATISTIC,
                p.value = PVAL,
                residuals=datall)
    }else{
        RVAL = new('lev',
                formula=form,
                statistics = STATISTIC,
                p.value = PVAL,
                residuals=datall)
    }


    RVAL

}

lev.formula <- function(formula, data,...) {
        if(!missing(data)) try(detach(data),silent=T)
        if(missing(data)) stop("'data' must be defined")
        m <- match.call(expand.dots = FALSE)
        eframe <- parent.frame()
        if (is.matrix(md <- eval(m$data, eframe)))
                m$data <- md <- as.data.frame(data)
        dots <- lapply(m$..., eval, md, eframe)
        m$... <- NULL
        m[[1L]] <- as.name("model.frame")
        m <- as.call(c(as.list(m), list(na.action = NULL)))
        mf <- eval(m, eframe)

        form=formula(mf)
        vars1=all.vars(form)
        nvars=n(vars1)
        vars=vars1[2]
        for (i in 3:nvars) vars=paste(vars,"*",vars1[i],sep="")
        form=as.formula(paste(vars1[1],"~",vars,sep=""), env=.GlobalEnv)

        response <- attr(attr(mf, "terms"), "response")
        varnames <- names(mf)
        dep <- mf[[response]]
        xn <- varnames[-response]
        groups=interaction(mf[xn])
        list.op=list(y=dep, group=groups,form=form,source="form", ...)
        list.op
        do.call("lev.default", list.op)

}



lev.lm <- function(object,...) {
    dat=model.frame(object)
    m <- match.call(expand.dots = FALSE)
    vars1=all.vars(formula(object))
    nvars=n(vars1)
    vars=vars1[2]
    for (i in 3:nvars) vars=paste(vars,"*",vars1[i],sep="")
    formula=as.formula(paste(vars1[1],"~",vars,sep=""), env=.GlobalEnv)
    list.op=list(formula,data=dat,...)
	  #list.op
    do.call("lev.formula", list.op)

}




setClass("lev",
        representation(
                formula='formula',
                statistics ='numeric',
                p.value='numeric',
                residuals='data.frame')
)

setMethod("show", "lev",
        function(object){
            cat("\nTitle:\nLevene-type tests\n", sep = "")
            cat('\nFormula:\n')
            print(object@formula)
            cat("\nTests Result:\n")

            if (!is.null(object@statistics)) {
                statistics = object@statistics
                Names = names(statistics)
                cat("  STATISTICS:\n")
                for (i in 1:length(Names)) {
                    if (!is.na(statistics[i])) {
                        cat(paste("    ", Names[i], ": ",
                                        formatC(statistics[i], digits=4,width=10,format="f"), "\n", sep = "" ) )
                    }
                }
            }

            if (!is.null(object@p.value)) {
                pval = object@p.value
                Names = names(pval)
                if (Names[1] == "") space = "" else space = ": "
                cat("  P VALUE:\n")
                for (i in 1:length(Names)) {
                    if (!is.na(pval[i])) {
                        if (class(version) != "Sversion") {
                            cat(paste("    ", Names[i], space,
                                            formatC(pval[i], digits=4,width=10,format="f"), " \n", sep = "" ) )
                        } else {
                            cat(paste("    ", Names[i], space,
                                            formatC(pval[i], digits=4,width=10,format="f"), " \n", sep = "" ) )
                        }
                    }
                }
            }

        }
)

plot.lev=function(object, which=1L:4L, ylab=NULL,main=NULL, ...){
    if(!is.numeric(which) || any(which < 1) || any(which > 4))
        stop("'which' must be in 1:4")

    if(is.null(main))
        main=c("Levene","Robust Levene","Brown-Forsythe","O'Brien")
    if(is.null(ylab))
        ylab=c("residuals of mean","residuals of trimmed mean","residuals of median","r scrores")

    if(length(which)==1){
        par(mfrow=c(1,1))
    }else{
        if(length(which)==2){
            par(mfrow=c(1,2))
        }else{
            par(mfrow=c(2,2))
        }
    }

    for(i in which){
        boxplot(object@residuals[,i]~object@residuals$group,main=main[i], ylab=ylab[i], ...)
    }


}



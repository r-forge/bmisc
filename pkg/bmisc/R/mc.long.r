mc.long <- function (y,data, ...) {
        try(attach(data),silent=TRUE)
        UseMethod("mc.long")

}



mc.long.default = function( y, group, data, p.adjust.method="holm", column=NULL, silent=FALSE, ..., subset ) {

        test=deparse(substitute(group))
        test=substr(test[1],1,1)
        
        if(test=="s"){
                test=NULL
        }else{
                column1=as.vector(strsplit(deparse(substitute(group)), ", ")[[1]])
                ngr=length(column1)
                if(test=="p"){
                        column1[1]=substr(column1[1],7,nchar(column1[1]))
                }else{
                        if(test=="i"){
                                column1[1]=substr(column1[1],13,nchar(column1[1]))   
                        }else{
                                stop("Use 'paste' or 'interaction' for 'group'. See ?mc.long for examples on how to define 'group'")  
                        }
                }
                column1[ngr]=substr(column1[ngr],1,nchar(column1[ngr])-1) 
                
                if (!missing(data)){
                        if(!missing(subset)) {
                                m <- match.call(expand.dots = FALSE)
                                eframe <- parent.frame()
                                s <- eval(m$subset, data, eframe)
                                data=data[s,]
                                y.name=deparse(substitute(y))
                                y=data[[y.name]]
                                group <- interaction(data[column1])
                                group=as.factor(as.character(group))
                        }else{
                                y.name=deparse(substitute(y))
                                y=data[[y.name]]
                                group <- interaction(data[column1])  
                        }
                                
                        
                        
                }
                
                if (!is.numeric(y)) {
                        stop(y.name, " is not a numeric variable.")
                }    
        }
        
        
        if(is.null(column)){
                column=column1
        }
        
        
        if(is.null(p.adjust.method) | is.na(p.adjust.method)){
                p.adjust.method="holm"
        }
        
        p.t=as.data.frame((pairwise.t.test(y,group, p.adjust.method="none",...))$p.value)
        
        pair=pair.diff.default(y,group)
        mean.diff= data.frame(pair[[1]])
        se.diff <- data.frame(pair[[2]])
        
        
        comb=combn(levels(group),2)
        
        a=comb[1,]
        b=comb[2,]
        res=data.frame(a,b,stringsAsFactors=F)
        res$p.value=1
        nm=length(a)
        
        for (i in 1:nm){
                res$p.value[i]= p.t[rownames(p.t)==res$b[i],names(p.t)==res$a[i]]
                res$estimate[i]=mean.diff[rownames(p.t)==res$b[i],names(p.t)==res$a[i]]
                res$se[i]=se.diff[rownames(p.t)==res$b[i],names(p.t)==res$a[i]]
        }
        
        
        
        for (j in 1:2){
                for (i in 1:length(column)){
                        ncol=length(names(res))
                        name= paste("f",j,i,sep=".")
                        assign(name,t(as.data.frame(strsplit(res[,j], "\\.")))[,i])
                        res[,ncol+1]=t(as.data.frame(strsplit(res[,j], split="\\.")))[,i]
                }
        }
        
        try(detach(data), silent=T)
        if(!is.null(column)){
                colnames(res)[6:dim(res)[2]]= c(column, paste(column,1,sep="."))
        }
        
        res=data.frame(res[,c(6:length(res),3,4,5)])
        
        if(p.adjust.method!="none"){
                Name.padj=paste('p.adj',p.adjust.method, sep='.')
                res$p.adj=p.adjust(res$p.value, method = p.adjust.method, ... )
                names(res)[ncol(res)]=Name.padj
        }
        
        res.out=res
        for(i in (length(column)*2+1): max(length(names(res)))){
                res.out[,i]=sprintf("%.5f", res[,i])
        }

        res.out

        
        
}



mc.long.formula = function(formula,  data = parent.frame(), ..., subset)
{       
        try(detach(data), silent=T)
        
        m <- match.call(expand.dots = FALSE)
        eframe <- parent.frame()
        if (is.matrix(md <- eval(m$data, eframe)))
                m$data <- md <- as.data.frame(data)
        dots <- lapply(m$..., eval, md, eframe)
        m$... <- NULL
        m[[1L]] <- as.name("model.frame")
        m <- as.call(c(as.list(m), list(na.action = NULL)))
        mf <- eval(m, eframe)
        
        if (!missing(subset)) {
                s <- eval(m$subset, data, eframe)
                ## need the number of points before subsetting
                if(!missing(data)) {
                        l <- nrow(data)
                } else {
                        mtmp <- m
                        mtmp$subset <- NULL
                        l <- nrow(eval(mtmp, eframe))
                }
                dosub <- function(x) if (length(x) == l) x[s] else x
                dots <- lapply(dots, dosub)
        }
        response <- attr(attr(mf, "terms"), "response")
        
        varnames <- names(mf)
        y <- mf[[response]]
        xn <- varnames[-response]
        group=interaction(mf[xn])
        list.op=list(y=y, group=group,column=xn, dots)
        do.call("mc.long", list.op)
        
}

mc.long.lm <- function(y, subset=NULL, ...) {
        try(detach(data), silent=T)
        dat=model.frame(y)
        m <- match.call(expand.dots = FALSE)
        form=formula(y)
        list.op=list(form,data=dat, subset=m$subset,...)
        do.call("mc.long.formula", list.op)

}


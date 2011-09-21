mc.long <- function (y, data=NULL,...) {

   if (!is.null(data)) {attach(data)}

   if(class(y)=="formula" & is.null(data)) {
        stop("'data' should be indicated when using a formula.")
    }
  UseMethod("mc.long")

     try(if(!is.null(data)) {detach(data)}, silent=T)


}

mc.long.default = function(y, group,data=NULL, p.adjust.method="holm", column=NULL, silent=FALSE, ... ) {

  if (!is.numeric(y)) {
        stop(deparse(substitute(y)), " is not a numeric variable")
    }
  test=NULL
  if(is.null(column)){
    test=deparse(substitute(group))
    test=substr(test[1],1,1)
        if(test=="p"){
          column=as.vector(strsplit(deparse(substitute(group)), ", ")[[1]])
          ngr=length(column)
          column[1]=substr(column[1],7,nchar(column[1]))
          column[ngr]=substr(column[ngr],1,nchar(column[ngr])-1)
        }
        if(test=="i"){
          column=as.vector(strsplit(deparse(substitute(group)), ", ")[[1]])
          ngr=length(column)
          column[1]=substr(column[1],13,nchar(column[1]))
          column[ngr]=substr(column[ngr],1,nchar(column[ngr])-1)
        }
        if(!is.null(test) & test!="p" & test!="i"){
          stop("see ?mc.long for exemples on how to define 'group'")
        }
  }

    group <- as.factor(group)

    
  if(is.null(p.adjust.method)){
    stop("'p.adjust.method' should be the indicated.  See ?P.adjust")
  }
 
    p.t=as.data.frame((pairwise.t.test(y,group, p.adjust.method="none",...))$p.value)
#    p.t[lower.tri(p.t, TRUE)] <- p.adjust(p=p.t[lower.tri(p.t, TRUE)],method =p.adjust.method ,...)
#   }
#   if(!is.null(n)){
#    p.t=as.data.frame((pairwise.t.test(y,group, p.adjust.method="none", ...))$p.value)
#    p.t[lower.tri(p.t, TRUE)] <- p.adjust(p=p.t[lower.tri(p.t, TRUE)],method =p.adjust.method,n=n ,...)
#   }

  
  pair=pair.diff.default(y,group)
  mean.diff= data.frame(pair[[1]])
  se.diff <- data.frame(pair[[2]])

  try(if(!is.null(data)) {detach(data)}, silent=T)

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
      if(!is.null(test)){
        if(test=="i"){
          assign(name,t(as.data.frame(strsplit(res[,j], "\\.")))[,i])
          res[,ncol+1]=t(as.data.frame(strsplit(res[,j], split="\\.")))[,i]
        }
        if(test=="p"){
          assign(name,t(as.data.frame(strsplit(res[,j], " ")))[,i])
          res[,ncol+1]=t(as.data.frame(strsplit(res[,j], split=" ")))[,i]
        }
      }
      if(is.null(test)){
        assign(name,t(as.data.frame(strsplit(res[,j], "\\.")))[,i])
        res[,ncol+1]=t(as.data.frame(strsplit(res[,j], split="\\.")))[,i]
      }
    }
  }


  if(!is.null(column)){
    colnames(res)[6:dim(res)[2]]= c(column, paste(column,1,sep="."))
  }

  res=data.frame(res[,c(6:length(res),3,4,5)])

  if(p.adjust.method!="none"){
      res$p.adj=P.adjust(res$p.value, method = p.adjust.method, ... )
    }

  res.out=res
  for(i in (length(column)*2+1): max(length(names(res)))){
  res.out[,i]=sprintf("%.5f", res[,i])
  }
  cat("Multiple T test with ", p.adjust.method,"'s correction\n\n", sep="")
  print(res.out)
  res <-res
  

}



mc.long.formula = function(y,data=NULL,  ... ) {
  try(if(!is.null(data)) {detach(data)}, silent=T)
  try(if(!is.null(data)) {attach(data)}, silent=T)
  form=y
  dat=model.frame(form)
  group <- interaction(dat[,2:dim(dat)[2]], sep=".", drop=T)
   column = names(dat)[2:dim(dat)[2]]
    if (dim(dat)[2] == 2) {
        column = names(dat)[dim(dat)[2]]
    }

  y=dat[,1]

  try(if(!is.null(data)) {detach(data)}, silent=T)

  mc.long.default(y=y,group=group, column=column,...)

}

mc.long.lm <- function(y, ...) {
  dat=model.frame(y)
  y=formula(y)
  mc.long.formula(y=y,data=dat)

}


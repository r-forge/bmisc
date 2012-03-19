att.strp <- function (x) UseMethod("att.strp",x)

att.strp.default <- function(x) {
    if(!is.null(attributes(x))){
    atr <-names(attributes(x))
    for (i in atr) attr(x, i) <- NULL }
    return(x)
}


att.strp.matrix <- function(x){
    atr <-names(attributes(x))
    sel=atr[atr %nin% c("names","row.names","na.action", "class","dim","dimnames")]
    if(!is.null(sel)){
      if(n(sel)>=1) for(i in sel)attr(x, i) <- NULL }
    return(x)
}

att.strp.data.frame <- function(x){
    x=att.strp.matrix(x)
    nc=dim(x)[2]
    for(i in seq(nc)){
        x[,i]= att.strp(x[,i])
        }
    return(x)
}

att.strp.array <- function(x){
    x=att.strp.matrix(x)
    return(x)
    warning("Some attributes may still exist in this array. Try looping att.strp for all dimensions of the array.")
}

att.strp.list <- function(x){
    x=att.strp.matrix(x)
    nle=length(x)
    for(j in seq(nle)){
        x[[j]]= att.strp(x[[j]])
        }
    return(x)
}



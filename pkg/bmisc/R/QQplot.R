QQplot <- function (dat,...) UseMethod("QQplot")

QQplot.default <- function (dat, quant=TRUE,cex.q=2,norm=T,main=paste("Normal Q-QPlot of", DNAME), ...){  

  DNAME <- paste(deparse(substitute(dat), 500), collapse="\n")
	qqnorm(dat,main=main, ...)
	if(norm) {qqline(as.vector(dat))}
	if(quant){
	points( qnorm(c(.25,0.5,.75)),  quantile(dat, c(.25,0.5, .75), na.rm=TRUE) ,   pch=16, col=c("red","blue","red"),cex=cex.q) 
  }
}


QQplot.norm=function(object,...){
    QQplot(dat=object@data, main=paste("Normal Q-QPlot of", object@data.name,sep=" "),... )
    
}

QQplot.lm=function(object,...){
    QQplot(dat=residuals(object), main="Normal Q-QPlot of Residuals",... )
    
}

QQplot.glm=function(object,type="deviance",...){
    QQplot(residuals(object, type=type), main=paste("Normal Q-QPlot of", type,"residuals.",sep=" "),... )
}

QQplot.gam=function(object,type="deviance",...){
    QQplot(residuals(object, type=type), main=paste("Normal Q-QPlot of", type,"residuals.",sep=" "),... )
}




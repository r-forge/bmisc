Errbar <- function (x,y,...) UseMethod("Errbar")

Errbar.default <- function( x, y, 
                            xinf=NULL, 
                            xsup=NULL, 
                            yinf=NULL, 
                            ysup=NULL, 
                            xint=NULL,
                            yint=NULL, 
                            color="black", 
                            cap=0.05, ...){
     x=as.numeric(x)
     y=as.numeric(y)
    if(!is.null(yint) | !is.null(xint)){  
  	 if (!is.null(yint)){
        arrows(x,y,x,y+(yint/2),angle=90,length=.05,code=2,col=color,...)
        arrows(x,y,x,y-(yint/2),angle=90,length=.05,code=2,col=color,...)} 
      if (!is.null(xint)){
        arrows(x,y,x+(xint/2),y,angle=90,length=.05,code=2,col=color,...)
        arrows(x,y,x-(xint/2),y,angle=90,length=.05,code=2,col=color,...)}
     } 
      
      
    if((!is.null(yint) | !is.null(xint)) & (!is.null(yinf) | !is.null(ysup) | !is.null(xinf) | !is.null(xsup))){
      stop("The confidence interval method has priority over individually defined methods. see ?Errbar")}


	if (!is.null(yinf)){
		arrows(x,y,x,yinf,angle=90,length=cap,code=2,col=color,...)}
	if (!is.null(ysup)){
		arrows(x,y,x,ysup,angle=90,length=cap,code=2,col=color,...)}
	if (!is.null(xinf)){
		arrows(x,y,xinf,y,angle=90,length=cap,code=2,col=color,...)}
	if (!is.null(xsup)){
		arrows(x,y,xsup,y,angle=90,length=cap,code=2,col=color,...)}
		
}


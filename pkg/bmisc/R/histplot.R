histplot <- function (dat,...) UseMethod("histplot")

histplot.default <-
function(dat, breaks="Sturges", barc="steelblue", borc="white", fit.norm=TRUE, lcol="brown", stat=NULL, stat.lab=c("Mean","Median"),
  box=TRUE, rug=TRUE , main=paste("Histogram of" , DNAME),xlab=DNAME,...)
{

    DNAME <- paste(deparse(substitute(dat), 500), collapse="\n")


    hdat <- hist(dat, breaks=breaks, plot=FALSE)

# limits of graph

    xlim <- range(hdat$breaks)
    s = seq(xlim[1], xlim[2], length = 201)
    mean = mean(dat,na.rm=TRUE)
    sd = sd(dat,na.rm=TRUE)
    dens=dnorm(s, mean, sd)
    ylim <- c(0,  max(hdat$density , dens))


#Graph

    plot(hdat,freq=FALSE, col=barc ,border=borc,, xlab=xlab, xlim=xlim, ylim=ylim, main=main,...)
    abline(h = par("usr")[3], col = "black")

    if(box) {box()}

        if (fit.norm){
                mean = mean(dat,na.rm=TRUE)
                sd = sd(dat,na.rm=TRUE)
                xlim2 = range(hdat$breaks)
                s = seq(xlim2[1], xlim2[2], length = 201)
                lines(s, dnorm(s, mean, sd), lwd = 2, col = lcol)
    }
    
    if(!is.null(stat)){
    stat=match.arg(stat, c("all","mean","median"))
    switch(stat,
                                                             
           mean=  {  abline(v = mean (dat,na.rm=TRUE), lwd = 2, col = "black")          
                     Text = paste(stat.lab[1],":", signif(mean(dat,na.rm=TRUE), digits=3))  
                     mtext(Text, side = 4, adj = 0, col = "black",                      
                           cex = 0.7)},                                                 
           median={  abline(v = median (dat,na.rm=TRUE), lwd = 2, col = "black")        
                     Text = paste(stat.lab[2],":", signif(median(dat,na.rm=TRUE), digits=3))
                     mtext(Text, side = 4, adj = 0, col = "black",                      
                      cex = 0.7)},                                                 
           all=   {  abline(v = mean (dat,na.rm=TRUE), lwd = 2, col = "black", lty=3)   
                     abline(v = median (dat,na.rm=TRUE), lwd = 2, col = gray(0.4))
                     abline(v = mean (dat,na.rm=TRUE), lwd = 2, col = "black", lty=3)       
                     med = paste(stat.lab[2],":", signif(median(dat,na.rm=TRUE), digits=3)) 
                     mea = paste(stat.lab[1],":", signif(mean(dat,na.rm=TRUE), digits=3))   
                     mtext(mea, side = 4, adj = 0, col = "black",                       
                           cex = 0.7)                                                   
                     mtext(med, side = 4, adj = 0, col = gray(0.4),                     
                           cex = 0.7, line=0.7)}                                        
    )}                                                                               
                
        if (rug) {rug(dat, ticksize = 0.01, quiet = TRUE)
    }
}

histplot.norm=function(object,...){
    histplot(object@data, main=paste("Histogram of", object@data.name,sep=" "),xlab=object@data.name,... )   
}

histplot.lm=function(object,type="response",...){
    histplot(residuals(object), main="Histogram of Residuals",xlab=paste("Residuals (",type,")",sep=""),... )   
}

histplot.glm=function(object,type="deviance",...){
    histplot(residuals(object, type=type), main="Histogram of Residuals",xlab=paste("Residuals (",type,")",sep=""),... )
}

histplot.gam=function(object,type="deviance",...){
    histplot(residuals(object, type=type), main="Histogram of Residuals",xlab=paste("Residuals (",type,")",sep=""),... )
}

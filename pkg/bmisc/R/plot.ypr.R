plot.ypr <- 
        function(data, 
                main='Yield per Recruit &\n SSB per Recruit', 
                ylab.ypr='Yield per Recruit (YPR)',
                ylab.ssb='Spawning Stock Biomass per Recruit (SSB/R)',
                xlab='Fishing Mortality (F)',
                col.ypr='blue',
                col.ssb='red', 
                ref=TRUE,
                legend=TRUE){
    
    par(mar=c(5,4,4,4.1))
    ylim1=c(0,max(data$ypr$ypr)*1.1)
    ylim2=c(0,max(data$ypr$ssb)*1.1)
    plot(ypr~F, data=data$ypr,main=main,ylim=ylim1, 
            ylab=ylab.ypr,xlab=xlab,type='l', lwd=3, col=col.ypr, las=1)
    sels=data$ref.line.sel
    if(ref){
        for(i in 1:dim(sels)[1]){
            lines(c(-1,ypr)~c(F,F),data=data$ypr[sels[i,],], lty=2)
        }
        
        points(ypr~F,data=data$ypr[sels[,1],], pch=21, col='black', bg='white',cex=1.2)
        y.coord=par('usr')[2]*0.01
        r.names=rownames(sels)
        text(x=data$ypr$F[sels[1,]], y=y.coord, labels=r.names[1], srt=90,adj=c(0.2,1.2) , cex=0.8, font=2)
        text(x=data$ypr$F[sels[2,]], y=y.coord, labels=r.names[2], srt=90,adj=c(0.2,-0.4), cex=0.8, font=2)
        text(x=data$ypr$F[sels[3,]], y=y.coord, labels=r.names[3], srt=90,adj=c(0.2,1.2) , cex=0.8, font=2)
    }
    
    par(new=TRUE)
    plot(ssb~F, data=data$ypr,type='l',xaxt="n",yaxt="n",xlab="",ylab="", lwd=3, col=col.ssb, ylim=ylim2)
    
    if(ref)points(ssb~F,data=data$ypr[sels[,1],], pch=21, col='black', bg='white',cex=1.2)
    
    axis(4, las=1)
    mtext(ylab.ssb,side=4,line=2.9)
    if(legend){
        legend("topright",col=c("blue","red"),lty=1, lwd=3,legend=c("YPR","SSB/R"),
                horiz=TRUE, bty='o', bg='white')
    }
}


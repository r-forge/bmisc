suiv.age.an <- function(data, line=F, poly=F){

  ###  moyenne  ###
		par(mar=c(5,4,4,5))
		plot(longueur..mm.~year, data=data, pch=NA, main=paste("Longueur moyenne par âge\npour chaque années (",j,")", sep=""), ylab="Longueur moyenne (mm)", xaxt="n")
		axis(side=1, at=min(years$year):max(years$year))

		limit=par("usr")
		yrs=years$year[is.odd(years$year)]
		if(poly){
      for(k in yrs){
        	 	arr=0.5
        		polygon(x=c(k-arr,k+arr,k+arr,k-arr), y=rep(c(limit[3],limit[4]), each=2), col=gray(0.9), border=NA)
    	 }
    	}
    	box()

		for(i in age){
			data2=summaryBy(longueur..mm.~year, data=data[data$age==i,], FUN=mean, na.rm=T, keep.names=T)
			data3=merge(years,data2, all=T)
			param2=param[param$age==i,]
			if(line){
        points(longueur..mm.~year, data=data2,type="o", lty=3,col=param2$col, pch=param2$pch, bg=param2$col, lwd=1.5)
        points(longueur..mm.~year, data=data3,type="o", col=param2$col, pch=param2$pch, bg=param2$col, lwd=1)
      }else{
        points(longueur..mm.~year, data=data2,col=param2$col, pch=param2$pch, bg=param2$col)
        points(longueur..mm.~year, data=data3, col=param2$col, pch=param2$pch, bg=param2$col)
      }
		}


      legend(x=limit[2], y=limit[4], legend=age, cex=1, pt.cex=1.3, col=param$col, pt.bg=param$col, pch=param$pch,horiz=F, title="Âges", xpd=T)

	###  médianne  ###
		par(mar=c(5,4,4,5))
		plot(longueur..mm.~year, data=data, pch=NA, main=paste("Longueur médianne par âge\npour chaque années (",j,")", sep=""), ylab="Longueur médianne (mm)", xaxt="n")
		axis(side=1, at=min(years$year):max(years$year))

		limit=par("usr")
		yrs=years$year[is.odd(years$year)]
 		if(poly){
  		for(k in yrs){
        		arr=0.5
        		polygon(x=c(k-arr,k+arr,k+arr,k-arr), y=rep(c(limit[3],limit[4]), each=2), col=gray(0.9), border=NA)
     		 }
      }
    	box()


		for(i in age){
			data2=summaryBy(longueur..mm.~year, data=data[data$age==i,], FUN=median, na.rm=T, keep.names=T)
			data3=merge(years,data2, all=T)
			param2=param[param$age==i,]
			if(line){
        points(longueur..mm.~year, data=data2,type="o", lty=3,col=param2$col, pch=param2$pch, bg=param2$col, lwd=1.5)
        points(longueur..mm.~year, data=data3,type="o", col=param2$col, pch=param2$pch, bg=param2$col, lwd=1)
      }else{
        points(longueur..mm.~year, data=data2,col=param2$col, pch=param2$pch, bg=param2$col)
        points(longueur..mm.~year, data=data3, col=param2$col, pch=param2$pch, bg=param2$col)
      }
		}


		legend(x=limit[2], y=limit[4], legend=age, cex=1, pt.cex=1.3, col=param$col, pt.bg=param$col, pch=param$pch,horiz=F, title="Âges", xpd=T)

}
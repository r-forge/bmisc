graph_r.m <- function(data, line=F, plot=c('rm','r','m'), main=NULL, lab=T){

  plot=match.arg(plot)
  
  xlim=c(min(data$year, na.rm=T),max(data$year, na.rm=T))
  if(lab){
    xlim=c(min(data$year, na.rm=T),max(data$year, na.rm=T))
    xlim[2]=xlim[2]+((xlim[2]-xlim[1])/20)
  }

  
  if(plot=='rm' | plot=='r'){
    title=main[1]
    if(is.null(main)){
      title=paste(deparse(substitute(data)),'\nNo.Relevé',sep='')
    }
  	plot(no_rel~year, data=data, xaxt='n', ylab='numéro de relevé',xlim=xlim, xlab='Année', main=title)
	  axis(side=1,min(data$year):max(data$year))
	  #axis(side=2, seq(min(data$no_rel),max(data$no_rel),1),las=2 )
	  if(line){
      abline(h=unique(data$no_rel),v=unique(data$year), lty=c(3,2), col="gray")
    }
  	points(no_rel~year, data=data,pch=19)
  	if(lab){
      text( y=data$no_rel, x=data$ye,labels= data$no_rel, font=1, adj=c(-0.4,0.7))
    }
  }

  if(plot=='rm' | plot=='m'){
    title=main[2]
    if(is.null(main) | class(main[2])=='logical' ){
      title=paste(deparse(substitute(data)),'\nMois',sep='')
    }

	plot(month~year, data=data, xaxt='n', ylab='Mois du relevé',xlim=xlim, xlab='Année', main=title)
	axis(side=1,min(data$year):max(data$year))
	#axis(side=2, seq(min(data$month, na.rm=T),max(data$month,na.rm=T),1),las=2 )
	if(line){
  		abline(h=unique(data$month),v=unique(data$year), lty=c(3,2), col="gray")
  	}
	points(month~year, data=data,pch=19)

	}
}
pds_long <- function(data, line=F,lab=F){

		data2= data[data$year==i ,]
		if(is.null(data$pds_tot..kg)){
		    ylim=c(min(data$pds_tot..g., na.rm=T), max(data$pds_tot..g., na.rm=T))
		    xlim=c(min(data$longueur..mm., na.rm=T),max(data$longueur..mm., na.rm=T))
		    if(lab){
		        ylim=c(min(data$pds_tot..g., na.rm=T)*0.9, max(data$pds_tot..g., na.rm=T)*1.1)
		        xlim=c(min(data$longueur..mm., na.rm=T)*0.9,max(data$longueur..mm., na.rm=T)*1.1)
		    }

		    plot(pds_tot..g.~longueur..mm., data=data2, main=i, ylab= 'Poids (g)', xlab='Longueur (mm)', xlim=xlim, ylim=ylim)
		    if(line){
		        abline(v=data2$longueur..mm., h=data2$pds_tot..g., lty=3, col=gray(0.75))
		    }
		    if(lab){
  		        text( data2$pds_tot..g.~data2$longueur..mm., labels= data2$longueur..mm., adj=c(-0.2,0), cex=0.8, font=2)
            }
        }else{
            ylim=c(min(data$pds_tot..kg., na.rm=T), max(data$pds_tot..kg., na.rm=T))
		    xlim=c(min(data$longueur..mm., na.rm=T),max(data$longueur..mm., na.rm=T))
		    if(lab){
		        ylim=c(min(data$pds_tot..kg., na.rm=T)*0.9, max(data$pds_tot..kg., na.rm=T)*1.1)
		        xlim=c(min(data$longueur..mm., na.rm=T)*0.9,max(data$longueur..mm., na.rm=T)*1.1)
		    }

		    plot(pds_tot..kg.~longueur..mm., data=data2, main=i, ylab= 'Poids (kg)', xlab='Longueur (mm)', xlim=xlim, ylim=ylim)
		    if(line){
		        abline(v=data2$longueur..mm., h=data2$pds_tot..kg., lty=3, col=gray(0.75))
		    }
		    if(lab){
  		        text( data2$pds_tot..kg.~data2$longueur..mm., labels= data2$longueur..mm., adj=c(-0.2,0), cex=0.8, font=2)
            }
        }
}
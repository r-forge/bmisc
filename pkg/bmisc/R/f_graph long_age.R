long.age <- function(data, line=F, plot=c('sb','s','b')){

	plot=match.arg(plot)

    data2= data[data$year==i,]
	xlim=c(min(data$age, na.rm=T)*0.9, max(data$age, na.rm=T)*1.1)
	ylim=c(min(data$longueur..mm., na.rm=T)*0.9,max(data$longueur..mm., na.rm=T)*1.1)

	if(plot=='s' | plot=='sb'){
		plot(longueur..mm.~age, data=data2, main=i, xlab= 'Age', ylab='Longueur (mm)', xlim=xlim, ylim=ylim, pch=NA,xaxt='n')
		if(line){
			abline(h=data2$longueur..mm., v=data2$age, lty=3, col=gray(0.75))
		}
		points(longueur..mm.~I(age-0.15), data=data2[data2$sexe==1,], col="cornflowerblue", pch=19, cex=0.7)
		points(longueur..mm.~I(age+0), data=data2[data2$sexe==5,], col="darkgoldenrod3", pch=19, cex=0.7)
		points(longueur..mm.~I(age+0.15), data=data2[data2$sexe==9 |is.na(data2$sexe),], col="gray", pch=19, cex=0.7)
		#text( data2$longueur..mm.~data2$age, labels= data2$longueur..mm., adj=c(-0.2,0), cex=0.8, font=2)
		rang=range(round(xlim))
    	axis(side=1, at=rang[1]:rang[2])
		legend ("topleft",legend=c("Mâles (1)","Femelles (5)","Indéterminé (9 ou NA)" ),cex=1.3,
				pt.cex=1.3,pch=c(21,21,21), pt.bg=c("cornflowerblue","darkgoldenrod3","gray"), horiz=F,bg="white")
    }

    if(plot=='b' | plot=='sb'){
		m=summaryBy(longueur..mm.~age+sexe, data=data2, FUN=mean, na.rm=T)

		plot(longueur..mm.~age, data=data2, main=i, xlab= 'Age', ylab='Longueur (mm)', xlim=xlim, ylim=ylim, pch=NA, xaxt='n')
    	if(line){
    		abline(h=m$longueur..mm..mean,v=data2$age,lty=3, col=gray(0.75))
		}
    	if(any(data2$sex == "mâle")){
			boxplot(longueur..mm.~age, data=data2,add=T, cex=0.4 ,pch=8,
			        boxwex = 0.15, at = sort.vdf(unique(data2$age[data2$sex == "mâle"]))-0.2,
			        subset = sex == "mâle", col = "cornflowerblue",
			        main = i,
			        xlab = "Âge",
			        ylab = "Longueur (mm)",
			        xlim = xlim, ylim = ylim, yaxs = "i", xaxt='n')
		}
    	if(any(data2$sex == "femelle")){
			boxplot(longueur..mm.~age, data=data2,add=T , cex=0.4 ,pch=8,
			        boxwex = 0.15, at = sort.vdf(unique(data2$age[data2$sex == "femelle"])),
			        subset = sex == "femelle", col = "darkgoldenrod3", xaxt='n')
		}
		if(any(data2$sex == "indéterminé")){
			boxplot(longueur..mm.~age, data=data2,add=T , cex=0.4 ,pch=8,
			        boxwex = 0.15, at = sort.vdf(unique(data2$age[data2$sex == "indéterminé"]))+0.2,
			        subset = sex == "indéterminé", col = "gray", xaxt='n')
		}

		legend ("topleft",legend=c("Mâles (1)","Femelles (5)","Indéterminé (9 ou NA)" ),cex=1.3,
				pt.cex=1.3,pch=rep(15,3), col=c("cornflowerblue","darkgoldenrod3","gray"), horiz=F,bg="white")

		rang=range(round(xlim))
		axis(side=1, at=rang[1]:rang[2])
	}
}
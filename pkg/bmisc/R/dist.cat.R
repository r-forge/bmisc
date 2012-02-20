dist.cat <- function(formula, 
        data,
        boxp=TRUE, 
        densp=TRUE, 
        histp=FALSE,
        n.level=NULL,
        cex.stat=0.8,
        stats=F,
        lpos=c("right","left"),
        save.fig=FALSE, 
        fty=c('wmf','png'),
        file=NULL,
        xlab, 
        ylab,
        xlim,
        ylim,
        ref,
        ref.col=c("red","blue"),  ... )
{
        
        if('bmisc' %nin% (.packages()) ) library(bmisc)
        lpos=match.arg(lpos)
        fty=match.arg(fty)

        
        vars=all.vars(formula)
        c.a=which(names(data)==vars[1])
        c.b=which(names(data)==vars[2])
        
        yval=list(unique(data[,c.a]))
        if(!is.numeric(yval)){
                ytest=TRUE
                yval2=yval
                yval=list(as.numeric(as.factor(yval[[1]])))
                y=as.numeric(as.factor(data[,vars[1]]))
        }
        
        n.vec=1
        if(!is.null(n.level)){
                yval1=min(yval[[1]]):max(yval[[1]])
                yu=sort(unique(data[,c.a]))
                sel=which(yval1 %nin% yu)
                yval2=yval1
                yval2[sel]=NA
                n.vec=1:ceiling(n(yval1)/n.level)
                vec=rep(n.vec,each=n.level)[seq(n(yval1))]
                yval=split(yval2, vec)
        }
        
        if(missing(xlim)){
                minvec=vector()
                maxvec=vector()
                for(l in unique(data[,c.a])){
                        #l="20A8"
                        dat1=data[,c.b][data[,c.a]==l]
                        if(n(dat1)>1){
                                dat2=range(density(dat1)$x)
                        }
                        mmvec[l]=dat2
                }
                
                
                xlim=range(mmvec,max(data[,c.b],na.rm=T),min(data[,c.b],na.rm=T), na.rm=T)
        }
        
        if(lpos=="right" & stats){ xlim[2]=(xlim[2]-xlim[1])/5 +xlim[2]}
        if(lpos=="left" & stats){xlim[1]=xlim[1]*0.8}
        
        for(j in n.vec){
                
                if(save.fig & is.null(file)){stop("The 'file' option must be spcefied when 'save.file' is TRUERUE. See ?distrib.")}
                if(save.fig & fty=="wmf"){win.metafile(filename=paste(j,file, sep=''),width=11, height=8.5, restoreConsole = TRUE)}
                if(save.fig & fty=="png"){png(filename=paste(j,file, sep=''),width=11, height=8.5,units ="in", res=150, restoreConsole = TRUE)}
                if(ytest){
                        ylim=c(min(yval[[j]],na.rm=TRUE),max(yval[[j]],na.rm=TRUE)+0.9)
                }else{
                        ylim=c(min(yval[[j]],na.rm=TRUE),max(yval[[j]],na.rm=TRUE)+0.9)
                }
                if(missing(ylab)){ylab=names(data)[c.a]}
                if(missing(xlab)){xlab=names(data)[c.b]}
                
                
                plot(1, type='n',xlim=xlim,ylim=ylim, yaxt='n',xlab=xlab, ylab=ylab,bg="white",...)
                limit=par('usr')
                #polygon(x=c(limit[1],limit[2],limit[2],limit[1]),y=c(limit[3],limit[3],limit[4],limit[4]), col="white")

                #abline(v=ceiling(seq(xlim[1],xlim[2],100)), col='gray')
                #axis(side=1,at=seq(0,10000,100))
                if(ytest){
                        axis(side=2,at=yval[[1]], labels=yval2[[1]], las=2,...)
                }else{
                        axis(side=2,at=ylim[1]:ylim[2], las=2,...)
                }
                
                yseq=sort(yval[[j]][!is.na(yval[[j]])])
                for(i in yseq){
                        abline(h=i,col='gray')
                        
                        if(ytest){
                                sel=which(y==i)
                                long=data[c.b][y==i,]
                        }else{
                                long=data[c.b][data[c.a]==i]
                        }
                        
                        
                        
                        if(n(long)<2) points(y=i,x=long)
                        
                        if(n(long)>1){
                                
                                if(histp){
                                        p=hist(long,  plot=F)
                                        px=p$breaks
                                        py=p$density
                                        k=0.8/max(py, na.rm=TRUE)
                                        py=(p$density *k)+i
                                        pox=c(1,2,2,1)
                                        poy=c(1,1)
                                        nbar=n(p$mids)
                                        for(z in 1:nbar){
                                                polygon(x=px[pox+z-1], y=c(i,i,py[poy+z-1])  )
                                        }
                                }
                                
                                
                                dens=try(density(long))
                                k=0.8/max(dens$y, na.rm=TRUE)
                                dens$y=(dens$y*k)+ i
                                m=mean(long, na.rm=TRUE)
                                std2=2*sd(long)
                                inf=m-std2
                                sup=m+std2
                                
                                if(densp){
                                        points(x=dens$x, y=dens$y, type='l')
                                        seld=which(dens$x>ref)
                                        seld1=c(which(dens$x<ref),seld[1])
                                        polygon(x=c(dens$x[seld],rev(dens$x[seld])), 
                                                y=c(dens$y[seld],rep(last(dens$y[seld]),n(dens$x[seld]))),
                                                 col=ref.col[2])
                                        polygon(x=c(dens$x[seld1],rev(dens$x[seld1])), 
                                                y=c(dens$y[seld1],rep(dens$y[seld1][1],n(dens$x[seld1]))),
                                                 col=ref.col[1])
                                }
                                
                                sel=which.min(abs(dens$x-m))
                                sel.inf=which.min(abs(dens$x-inf))
                                sel.sup=which.min(abs(dens$x-sup))
                                
                                #lines(x=c(m,m),y=c(min(dens$y),dens$y[sel]),lwd=3, col='darkgreen')
                                #lines(x=c(inf,inf),y=c(i-0.1,dens$y[sel.inf]),lwd=3, col='darkgreen')
                                #lines(x=c(sup,sup),y=c(i-0.1,dens$y[sel.sup]),lwd=3, col='darkgreen')
                                
                                box=boxplot(long,col=gray(0.7), boxwex=0.2,staplewex = 1.5,staplelwd=3, whisklwd=2,
                                        at=i, add=TRUE, horizontal=TRUE, cex=0.8, pch=19, outcol='red', outpch=8, xaxt='n', plot=boxp)
                                
                                #outliers=long[long<inf |long>sup | long< box$stats[1] | long> box$stats[5]]
                                #points(x=outliers, y=rep(i, n(outliers)), pch=8, cex=0.8, col='red')
                                
                                if(stats){
                                        m.vals=c(paste('moy =',round(m  , digits=1),sep=' '),
                                                paste('inf =',round(inf, digits=1),sep=' '),
                                                paste('sup =',round(sup, digits=1),sep=' '),
                                                paste('med =',round(box$stats[3]  , digits=1),sep=' '),
                                                paste('inf =',round(box$stats[1], digits=1),sep=' '),
                                                paste('sup =',round(box$stats[5], digits=1),sep=' '))

                                        xpos=range(dens$x)
                                        
                                        if(lpos=="right"){p=xpos[2]}else{p=xpos[1]}
                                        legend(x=p, y=i, legend=m.vals,xpd=TRUE,xjust=0, yjust=0, cex=cex.stat,
                                                ncol=2, x.intersp=0,y.intersp=1, adj=c(0,1), bty='n')
                                }
                                
                        }
                }
                
                if(save.fig){graphics.off()}
        }
}
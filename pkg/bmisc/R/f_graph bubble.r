bub <- function(form,data,z, sub.sex=NULL, type="perceptual",
                                  stand.num=10,
                                  stand.rad=0.3,
                                  symbol.fg=par("col"), symbol.bg=rgb(0,0,0,0.10),
                                  ...) {
                                  
                                  
    var.z=deparse(substitute(z))
    vars=all.vars(form)
    x.n=which(names(data)==vars[2])
    y.n=which(names(data)==vars[1])
    z.n=which(names(data)==var.z)
    xlim=range(data[,x.n])
    ylim=range(data[,y.n])
    data2=data
    if(!is.null(sub.sex)){
      data2=subset(data,sex==sub.sex)
    }

    plot(form, data=data2,type='n',  xaxt='n', yaxt='n', xlim=xlim, ylim=ylim,...)
    axis(side=1,at=xlim[1]:xlim[2])
    axis(side=2, at=ylim[1]:ylim[2],las=2)

    
    if (is.na(stand.num)) stand.num <- max(z,na.rm=T)
    spread.x <- par("usr")[2] - par("usr")[1]
    spread.y <- par("usr")[4] - par("usr")[3]
    ratio.x.y <- spread.x / spread.y
    stand.rad <- stand.rad * 0.05*spread.x
    z=data2[,z.n]


    switch(type,
            volume = scale <- ((z/stand.num)^(1/3))*stand.rad,
            surface = scale <- sqrt(z/stand.num)*stand.rad,
            perceptual   = scale <- ((z/stand.num)^0.57)*stand.rad)
    
    
    symbols(y=data2$age, x=data2$year, circle=scale,inches=F, bg=symbol.bg, add=T)

}




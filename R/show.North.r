show.North <- function (pos=NULL,...) UseMethod("show.North")

show.North.default <- function(pos=NULL , arrow.col="black", arrow.fill="black",
                      N.cex=1, arrow.lwd=1, N.family="HersheyGothicEnglish") {

                    
  val=c("topright","topleft","bottomright","bottomleft")                    
	xpd=F
	
  limits <- par("usr")
	rx <- abs(limits[2] - limits[1])
	ry <- abs(limits[4] - limits[3])

  if(is.null(pos)){
                    where="topright"}
                    	
	if(is.numeric(pos)){
                    where="pos"}
                    
  if(is.character(pos)){
                    where=pos}
                    
  if(is.character(pos) & !any(pos==val)){
                    stop("'pos' can ONLY be one of c('topright','topleft','bottomright','bottomleft')")   }
	
  if(where=="pos" & (!all(pos<=1) | !all(pos>=0))){
                    warning("\n 'pos' values have to be between 0-1.\n North arrow is outside the plotting region.")
                    xpd=T}
                        
		switch(where,
		  pos={
		    x1=limits[1]+pos[1]*rx
        x2 <- x1 - 0.015*rx
      	y1 <- limits[3]+pos[2]*ry
				y2 <- limits[3]+(pos[2]+0.02)*ry
				y3 <- limits[3]+(pos[2]+0.12)*ry
				y4 <- limits[3]+(pos[2]+0.08)*ry
				y5 <- limits[3]+(pos[2]+0.075)*ry
		  },
			topleft = {
				x1 <- limits[1]+0.06*rx  # pour le Nord
				x2 <- x1 - 0.015*rx
				y1 <- limits[3]+0.83*ry
				y2 <- limits[3]+0.85*ry
				y3 <- limits[3]+0.95*ry
				y4 <- limits[3]+0.91*ry
				y5 <- limits[3]+0.905*ry
			},
			topright = {
				x1 <- limits[2]-0.06*rx  # pour le Nord
				x2 <- x1 - 0.015*rx
				y1 <- limits[3]+0.83*ry
				y2 <- limits[3]+0.85*ry
				y3 <- limits[3]+0.95*ry
				y4 <- limits[3]+0.91*ry
				y5 <- limits[3]+0.905*ry
			},
			bottomleft = {
				x1 <- limits[1]+0.06*rx  # pour le Nord
				x2 <- x1 - 0.015*rx
				y1 <- limits[3]+0.1*ry
				y2 <- limits[3]+0.12*ry
				y3 <- limits[3]+0.22*ry
				y4 <- limits[3]+0.18*ry
				y5 <- limits[3]+0.175*ry
			},
			bottomright = {
				x1 <- limits[2]-0.06*rx  # pour le Nord
				x2 <- x1 - 0.015*rx
				y1 <- limits[3]+0.1*ry
				y2 <- limits[3]+0.12*ry
				y3 <- limits[3]+0.22*ry
				y4 <- limits[3]+0.18*ry
				y5 <- limits[3]+0.175*ry
			}
		)
	
	text(x1, y1, "N", cex=N.cex, family=N.family, xpd=xpd)
	segments(x1, y2, x1, y3, col=arrow.col, lwd=arrow.lwd, xpd=xpd)
	polygon(c(x1,x1,x2,x1), c(y4,y3,y5,y4), lwd=arrow.lwd, border=arrow.col, col=arrow.fill, xpd=xpd)
	invisible()
}
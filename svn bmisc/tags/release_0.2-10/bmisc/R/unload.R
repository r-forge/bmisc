unload <- function (pack=NULL) UseMethod("unload")

unload.default <- function(pack=NULL){
    if(is.null(pack)) {stop("Please specify which package(s) to unload")}
    
    installed <- pack %in% .packages(all.available = TRUE)
    ninst=pack[!installed]
    
    loaded=pack %in% (.packages())
    nload=list(pack[!loaded & installed])


    if(any(loaded)){cat(paste("\nThese packages have been unloaded -->>",list(pack[loaded]),"\n\n"  ))}
	
    
    if(any(!installed)){warning(paste("These packages are not installed -->>",ninst),call.=FALSE)}
    
    if(length(nload)>=1){warning(paste("These packages were not already loaded -->>",nload),call.=FALSE)}
    
    if(length(pack[loaded & installed])==0){stop("None of the packages are available to unload")}

    pack2=paste("package:", pack[loaded & installed], sep="")
    for(i in pack2)  {
      try(detach(i ,unload = TRUE, character.only=T))
      }
    }

    

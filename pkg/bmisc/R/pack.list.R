#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##      Creates a list of all packages installed                               ##
##           - Usefull for Rprofile.site (windows) or .Rprofile (Mac)          ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-08-04															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################


pack.list <- function(rprofile=FALSE)
{
        n.names=7
        library(utils)
        libs=installed.packages()[, 'Package']
        libs=att.strp(libs)
        
        repos=options("repos")
        if(is.null(repos) | is.na(repos)){
                op="options(repos=c('http://cran.skazkaforyou.com/','http://www.benoitr.comze.com/R/','http://download.walware.de/rj-0.5','http://www.stats.ox.ac.uk/pub/RWin/')) "
        }else{
                op=paste("options(repos=",paste(repos),")",sep="")
        }
        
        
        if(rprofile){
                sdir=vector()
                if (.Platform$OS.type== "unix") {
                        sdir=file.path("~/.Rprofile")
                }
                if (.Platform$OS.type=="windows") {
                        sdir=choose.files(default = "C:/Program Files/R/Rprofile.site", 
                                caption = "Select files 'Rprofile.site' in 'R/R-x.xx.x/etc' folder",
                                filters = matrix(c("Rprofile (*.site)", "*.site"), ncol=2), 
                                multi = FALSE)
                }                
                
                sel=seq(1,n(libs),n.names)
                pack=vector()
                for(i in sel){

                        if(i >=sel[2]){
                                if(i==length(libs)){
                                        packs=paste("               '",libs[i],"'", sep="")     
                                }else{
                                        packs=paste("               '",libs[i],"',", sep="")
                                }
                                
                        }else{
                                packs=paste("'",libs[i],"',", sep="")
                                
                                
                        }
                        
                        if(i!=length(libs)){
                                for(j in (i+1):min(c((i+n.names-1),length(libs))) ) {
                                        if(j!=length(libs)){
                                                packs=paste(packs,"'",libs[j],"',",sep="")  
                                        }else{
                                                packs=paste(packs,"'",libs[j],"'",sep="") 
                                        }
                                }
                        }
                        pack=c(pack,packs)
                }
                pack[1]=paste("necessary <- c(",pack[1], sep="")
                pack[length(pack)]=paste(pack[length(pack)],")",sep="")
                
                t1="## DEFINE REPOSITORIES"
                t2="## LIST OF NECESSARY PACKAGES"
                t3="## INSTALL NECESSARY PACKAGES"
                inst=c("library(utils,quietly = T,warn.conflicts =F)","installed <- necessary %in% installed.packages()[,'Package']",
                        "if (length(necessary[!installed]) >=1)","    install.packages(necessary[!installed])")
                
                packs=c(t1,op,"",t2,pack,"",t3,inst)
                
                fileConn<-file(sdir)
                options(warn=-1)
                rprofile=try(readLines(fileConn,warn = FALSE), silent=TRUE)
                options(warn=0)
                
                if(class(rprofile)=="try-error"){
                        writeLines(packs, fileConn)
                }else{
                        start=grep("## DEFINE REPOSITORIES", rprofile)
                        end=grep("## INSTALL NECESSARY PACKAGES", rprofile)+5
                        if(length(start)==0 | length(end)==0){
                                text=c(rprofile[1:length(rprofile)],"", packs) 
                        }else{
                                if(length(rprofile)>end){
                                        text=c(rprofile[1:(start-1)], packs,rprofile[end:length(rprofile)]) 
                                }else{
                                        if(start==1){
                                                text=c(rprofile[0], packs) 
                                        }else{
                                                text=c(rprofile[1:(start-1)], packs) 
                                        }
                                }
                        }
                        writeLines(text, fileConn) 
                }
                close(fileConn)
                if(sdir=="")stop("Operation canceled by user.", call. = FALSE)
                warning(paste("'",sdir,"' has been updated.",sep=''),call. = FALSE)
                
        }else{    
                sel=seq(n.names,n(libs),n.names)
                pack="necessary <- c("
                for(i in libs){
                        if(i==last(libs)){ pack=paste(pack," '",i,"')", sep="") 
                        }else{if(i%in%libs[sel]){pack=paste(pack," '",i,"',\n               ",sep="")
                                }else{pack=paste(pack," '",i,"',",sep="")}} 
                }
                
                t1="## LIST OF NECESSARY PACKAGES"
                t2="## INSTALL NECESSARY PACKAGES"
                inst="library(utils,quietly = T,warn.conflicts =F)\ninstalled <- necessary %in% installed.packages()[,'Package']\nif (length(necessary[!installed]) >=1)\n    install.packages(necessary[!installed])"
                packs=paste(t1,"\n",pack,"\n\n",t2,"\n",inst,sep="")
                fil=vector()
                if (.Platform$OS.type== "unix") {
                        fil=tclvalue(tkgetSaveFile(title='Save File',initialdir ='~/',initialfile = "List of installed R packages.txt", filetypes = "{{txt files} {.txt}} {{All files} .*}"))
                }
                if (.Platform$OS.type=="windows") {
                        fil=choose.files(default="C:/List of installed R packages.txt", 
                                caption = "Save File",
                                filters = matrix(c("Text (*.txt)", "*.txt"), ncol=2), 
                                multi = FALSE)
                }
                
                if(fil=="")stop("Operation canceled by user.", call. = FALSE)
                write(packs,file=fil)
                warning(paste("'",fil, "' has been created.", sep=''),call. = FALSE)
        }
}




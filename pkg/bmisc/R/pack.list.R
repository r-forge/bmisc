#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##      Creates a list (txt file) of all packages installed                    ##
##           - Usefull for Rprofile.site (windows) or .Rprofile (Mac)          ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-08-04															   ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################


pack.list <- function(n.names=7)
{
    library(utils)
    if (.Platform$OS.type== "unix") {sdir=file.path("~/package list.txt")}
    if (.Platform$OS.type=="windows") {sdir=file.path("C:/package list.txt")}
    libs=installed.packages()[, 'Package']
    libs=att.strp(libs)
    sel=seq(n.names,n(libs),n.names)
    pack="necessary <- c("
    for(i in libs){
        if(i==last(libs)){ pack=paste(pack," '",i,"')", sep="") 
        }else{if(i%in%libs[sel]){pack=paste(pack," '",i,"',\n               ",sep="")
            }else{pack=paste(pack," '",i,"',",sep="")}} 
    }
    
    fil=choose.files(default = sdir, caption = "Select files")
    write(pack,file=fil)
}

.First.lib <- function(libname, pkgname)
{

pkg_info <- utils::sessionInfo( package=pkgname )$otherPkgs[[ pkgname ]]
pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
v.name=as.data.frame(vignette(package = "bmisc")$results)$Item
packageStartupMessage("
  #####################################################################
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  ## bmisc ",pkg_info$Version,rep(" ",58-nchar(pkg_info$Version)),"##
  ##                                                                 ##
  # ----------------------------------------------------------------- #
  ## bmisc comes with ABSOLUTELY NO WARRANTY;                        ##
  ## It has different functions that I have accumulated with time.   ##
  ## This is an Alpha version.                                       ##
  # ----------------------------------------------------------------- #
  ##                                                                 ##
  ## Type vignette(\"bmisc\") to view the user's guides.               ##
  ## This package is still under construction.                       ##
  ##                                                                 ##
  ## Packaged on ",pkg_date,"                                         ##
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  #####################################################################"
  )


}
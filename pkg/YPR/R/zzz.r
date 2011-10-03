.First.lib <- function(libname, pkgname)
{

pkg_info <- utils::sessionInfo( package=pkgname )$otherPkgs[[ pkgname ]]
pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
v.name=as.data.frame(vignette(package = "YPR")$results)$Item
packageStartupMessage("
  #####################################################################
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  ## YPR ",pkg_info$Version,rep(" ",60-nchar(pkg_info$Version)),"##
  ##                                                                 ##
  # ----------------------------------------------------------------- #
  ## YPR comes with ABSOLUTELY NO WARRANTY;                        ##
  ## It has different functions for Yield Per Recruit evaluation. It ##
  ## is length based only for now but will also include weight-based ##
  ## model estimation in very soon.                                  ##
  ## This is an Alpha version.                                       ##
  # ----------------------------------------------------------------- #
  ##                                                                 ##
  ## Type vignette(\"YPR\") to view the user's guides.               ##
  ## This package is still under construction.                       ##
  ##                                                                 ##
  ## Packaged on ",pkg_date,"                                         ##
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  #####################################################################"
  )


}

?saveplot

.First.lib <- function(libname, pkgname)
{

pkg_info <- utils::sessionInfo( package=pkgname )$otherPkgs[[ pkgname ]]
pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
packageStartupMessage("
  #####################################################################
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  ## bmisc ",pkg_info$Version,rep(" ",58-nchar(pkg_info$Version)),"##
  ##                                                                 ##
  # ----------------------------------------------------------------- #
  ## bmisc comes with ABSOLUTELY NO WARRANTY;                        ##
  ## It has different functions that I have accumulated with time.   ##
  ## I am not the author of all of them even though I have           ##
  ## modified most of them. This is an Alpha version.                ##
  # ----------------------------------------------------------------- #
  ##                                                                 ##
  ## To view the user's guides 'bmisc.pdf' or 'relation_sel.pdf',    ##
  ## type vignette(\"bmisc\") or vignette(\"relation_sel\").             ##
  ## This package is still under construction.                       ##
  ##                                                                 ##
  ## Packaged on ",pkg_date,"                                         ##
  ##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
  #####################################################################"
  )


}
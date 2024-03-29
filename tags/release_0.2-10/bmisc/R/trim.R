###########################################################################
# trim() function is from gdata                                           #
# I have added it because there is some issues with the Perl dependencies #
#                              ###############                            #
# $Id: trim.R 989 2006-10-29 15:28:26Z ggorjan $                          #
###########################################################################

trim <- function(s, recode.factor=TRUE, ...)
  UseMethod("trim", s)

trim.default <- function(s, recode.factor=TRUE, ...)
  s

trim.character <- function(s, recode.factor=TRUE, ...)
{
  s <- sub(pattern="^ +", replacement="", x=s)
  s <- sub(pattern=" +$", replacement="", x=s)
  s
}

trim.factor <- function(s, recode.factor=TRUE, ...)
{
  levels(s) <- trim(levels(s))
  if(recode.factor) {                                                        
    dots <- list(x=s, ...)
    if(is.null(dots$sort)) dots$sort <- sort
    s <- do.call(what=reorder.factor, args=dots)
  }
  s
}

trim.list <- function(s, recode.factor=TRUE, ...)
  lapply(s, trim, recode.factor=recode.factor, ...)

trim.data.frame <- function(s, recode.factor=TRUE, ...)
{
  s[] <- trim.list(s, recode.factor=recode.factor, ...)
  s
}

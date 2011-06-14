rollmin <- function (x,k,...) UseMethod("rollmin")

rollmin.default <- function(x, k, na.pad = FALSE, align = c("center", "left", "right"), ...){
  n <- length(x) 
  rval <- rep(0, n) 
  a <- 0
  for (i in k:n) {
  rval[i] <- if (is.na(a) || is.na(rval[i=1]) || a==rval[i-1]) 
      min(x[(i-k+1):i]) # calculate max of window
  else 
      min(rval[i-1], x[i]); # max of window = rval[i-1] 
  a <- x[i-k+1] # point that will be removed from window
  }
  rval <- rval[-seq(k-1)]
  if (na.pad) {
    rval <- switch(match.arg(align),
      "left" = { c(rval, rep(NA, k-1)) },
      "center" = { c(rep(NA, floor((k-1)/2)), rval, rep(NA, ceiling((k-1)/2))) },
      "right" = { c(rep(NA, k-1), rval) })
  }
  return(rval)
}


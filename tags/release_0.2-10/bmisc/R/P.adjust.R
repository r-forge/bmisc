#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  This is a modified version p.adjust from stats.

P.adjust.methods <-
    c("holm", "hochberg", "hommel", "sidak", "bonferroni", "BH", "BY", "fdr", "none")

P.adjust <-
    function(p, method = P.adjust.methods, n = length(p))
{
    ## Methods 'Hommel', 'BH', 'BY' and speed improvements contributed by
    ## Gordon Smyth <smyth@wehi.edu.au>.
    method <- match.arg(method)
    if(method == "fdr") method <- "BH" # back compatibility

    p0 <- p
    if(all(nna <- !is.na(p))) nna <- TRUE
    p <- as.vector(p[nna])
 ## n <- length(p) ## -- maybe deprecate `n' argument ?
    #if(n !=length(p)){warning("   n != length(p); I hope you know what you are doing ! \nSee ?bmisc::P.adjust", call.=F)}
    if (n <= 1) return(p0)
    if (n == 2 && method == "hommel") method <- "hochberg"

    p0[nna] <-
      switch(method,
             bonferroni = pmin(1, n * p),
             sidak = (1-(1-p)^n)
             ,
             holm = {
               i <- 1L:n
               o <- order(p)
               ro <- order(o)
               pmin(1, cummax( (n - i + 1) * p[o] ))[ro]
             },
             hommel = { ## needs n-1 >= 2 in for() below
               i <- 1L:n
               o <- order(p)
               p <- p[o]
               ro <- order(o)
               q <- pa <- rep.int( min(n*p/(1L:n)), n)
               for (j in (n-1):2) {
                 q1 <- min(j*p[(n-j+2):n]/(2:j))
                 q[1L:(n-j+1)] <- pmin( j*p[1L:(n-j+1)], q1)
                 q[(n-j+2):n] <- q[n-j+1]
                 pa <- pmax(pa,q)
               }
               pmax(pa,p)[ro]
             },
             hochberg = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( (n - i + 1) * p[o] ))[ro]
             },
             BH = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               pmin(1, cummin( n / i * p[o] ))[ro]
             },
             BY = {
               i <- n:1
               o <- order(p, decreasing = TRUE)
               ro <- order(o)
               q <- sum(1/(1L:n))
               pmin(1, cummin(q * n / i * p[o]))[ro]
             },
             none = p)
    np= length(p0)
    for(i in 1:np){
      if(p0[i] > 1) {p0[i]=1}
    }
    p0
}

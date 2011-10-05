#################################################################################
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
##                                                                             ##
##     Descriptive Statistics for all DATA                                     ##
##                                                                             ##
##                                                                             ##
##                                                                             ##
## Author: Benoit Bruneau                                                      ##
## Date: 2011-06-18                                                            ##
##                                                                             ##
## FIXME LA FONCTION PLANTE                                                    ##
##                                                                             ##
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
#################################################################################

ttest.perm <- function (vec1,vec2,...) UseMethod("ttest.perm")

ttest.perm.default <- function(vec1, vec2, nperm=999, alternative = "two.sided", var.equal = T,  silent=FALSE, type="i", exact=FALSE){

switch(type,
        i = res <- t.perm(vec1, vec2, nperm=nperm, alternative = alternative, var.equal = var.equal,  silent=silent, exact=exact),
        p = res <- t.paired.perm(vec1, vec2, nperm=nperm, alternative=alternative, silent=silent,exact=exact))
}



##########################################################################################################
######################################### independant samples ############################################
##########################################################################################################

t.perm <- function(vec1, vec2, nperm=999, alternative = "two.sided", var.equal = T,  silent=FALSE, exact=FALSE){

if(!var.equal) nperm = 1
n1 <- sum(!is.na(vec1))
n2 <- sum(!is.na(vec2))
n  <- n1+n2

if(exact & n>20) stop("Exact test will not be used when n1+n2>20.  See ?ttest.perm", call.=FALSE)

var1 <- var(vec1)
var2 <- var(vec2)

tail <- match.arg(alternative, c("two.sided", "less", "greater"))

# Combine the two vectors into a single vector 'vec'
vec <- c(vec1,vec2)

# Compute the t-statistic for the unpermuted data

t.ref <- t.stat(n1,n2,vec1,vec2, var.equal=var.equal)
perm.t <- t.ref$t


# Compute the parametric p-value


if(alternative == "less" )       p.param <- pt(t.ref$t,df=t.ref$df,lower.tail=TRUE)
if(alternative == "greater" )    p.param <- pt(t.ref$t,df=t.ref$df,lower.tail=FALSE)
if(alternative == "two.sided" )  p.param <- 2*pt(-abs(t.ref$t),df=t.ref$df,lower.tail=TRUE)
                                                  


# Print these first results
if(!silent) cat('\n','\n',ifelse(alternative=="two.sided", "Bilateral",
                  ifelse(alternative=="less", "Unilateral (lesser than)",
                      ifelse( alternative=="greater", "Unilateral (greater than)", "ERROR in"))),' permutated t-test comparing two group means','\n',
                        ifelse(var.equal ==F,'for unequal variance','for equal variance'),'\n\n', sep="")

if(!silent) cat('Group sizes:',n1,n2,'\n')
if(!silent) cat('Group means:',t.ref$moy1,t.ref$moy2,'\n')
if(!silent) cat('Group variances:',var1,var2,'\n')
if(!silent) cat('t =',t.ref$t,'\n')
if(!silent) cat('df =',t.ref$df,'\n')
if(!silent) cat('pvalue (parametric) =',formatC(p.param,digits=5,width=7,format="f"),'\n')

# Perform the permutation test
nPGE <- 1

ngr=factorial(n1+n2)/(factorial(n1)*factorial(n2))

if(exact){
  if(n1!=n2 ){
   com=combn(vec,n1)
   com2=matrix(nrow=n2,ncol=ngr)
   for(i in 1:ngr){
     com2[,i]= vec[vec %in% com[,i]==F] 
   }
   nperm=ngr
  }
  if(n1==n2 ){
   com=combn(x=vec,m=n1)
   com2= com[,(ngr/2):1]
   com= com[,((ngr/2)+1):(ngr)]
   nperm=ngr/2
  }

  sub=sample(ncol(com),ncol(com))
  
for(i in sub)
   {
   #vec.perm  <-  sample(vec,n)
   #vec1.perm <- vec.perm[1:n1]
   #vec2.perm <- vec.perm[(n1+1):n]
   t.perm <- t.stat(n1,n2,com[,i],com2[,i], var.equal=var.equal)
   
   perm.t <- c(perm.t, t.perm$t)
    
   if(alternative == "less" ) if(t.perm$t <= t.ref$t) nPGE <- nPGE+1
   if(alternative == "greater" ) if(t.perm$t >= t.ref$t) nPGE <- nPGE+1
   if(alternative == "two.sided" ) if( abs(t.perm$t) >= abs(t.ref$t) ) nPGE <- nPGE+1

   }
}

if(!exact){
  if(nperm>(ngr/2) & n1==n2 ){
    nperm=ngr/2
    warning("nperm should be equal or smaller to ",(ngr/2), ".  See ?ttest.perm.",call. = FALSE)}
  if(nperm>(ngr) & n1!=n2 ){
    nperm=ngr
    warning("nperm should be equal or smaller to ",(ngr), ".  See ?ttest.perm.",call. = FALSE)}
    nPGE <- 1
  for(i in 1:nperm){
    vec.perm  <-  sample(vec,n)
    vec1.perm <- vec.perm[1:n1]
    vec2.perm <- vec.perm[(n1+1):n]
    t.perm <- t.stat(n1,n2,vec1.perm,vec2.perm, var.equal=var.equal)


    perm.t <- c(perm.t, t.perm$t)


    if(alternative == "less" ) if(t.perm$t <= t.ref$t) nPGE <- nPGE+1
    if(alternative == "greater" ) if(t.perm$t >= t.ref$t) nPGE <- nPGE+1
    if(alternative == "two.sided" ) if( abs(t.perm$t) >= abs(t.ref$t) ) nPGE <- nPGE+1

   }
}


# Print the permutational p-value
if(var.equal==T){P <- nPGE/(nperm+1)}
if(var.equal==F){P <- "\nThe Welch correction for t-tests is only available for parametric tests. \nThere is no known permutational equivalent.\n"}
if(!silent) cat('pvalue (',nperm,'permutations ) =',formatC(P,digits=5,width=7,format="f"),'\n')
if(!silent) cat('Alternative hypothesis:',tail,'\n','\n')

return(list(t.ref=t.ref$t, p.param=p.param, p.perm=P,
             nperm=nperm, t.perm=perm.t, var.x= var1, var.y=var2))
}


##########################################################################################################
############################################ paired samples ##############################################
##########################################################################################################

t.paired.perm <- function(vec1, vec2, nperm=999, alternative="two.sided", silent=FALSE, exact=FALSE){

if(exact){ warning("Exact test is not implemented yet in the paired t-test.  Be patient!",call. = FALSE) }

n1 <- length(vec1)
n2 <- length(vec2)
if(n1 != n2) stop("The two vectors have different lengths. They cannot be paired.")

tail <- match.arg(alternative, c("two.sided", "less", "greater"))

vec.by.rows = as.vector(t(cbind(vec1,vec2)))

res = t.test(vec1, vec2, paired=TRUE, alternative=tail)
t.ref =  res$statistic

# Print these first results
if(!silent) cat('\n','\n',ifelse(alternative=="two.sided", "Bilateral",
                  ifelse(alternative=="less", "Unilateral (lesser than)",
                      ifelse( alternative=="greater", "Unilateral (greater than)", "ERROR in"))),'permutated t-test comparing the means of two paired samples','\n','\n')
if(!silent) cat('Number of objects:',n1,'\n')
if(!silent) cat('Mean of the differences:',res$estimate,'\n')
if(!silent) cat('t :',t.ref,'\n')
if(!silent) cat('df:',res$parameter,'\n')
if(!silent) cat('pvalue (parametric):',formatC(res$p.value,digits=5,width=7,format="f"),'\n')

# Perform the permutation test
# Permutations are restricted to the two related observations for each object.
nPGE <- 1
for(i in 1:nperm)
	{
	vec.perm = rep(NA,2*n1)
	for(j in 1:n1) {
		i1 <- 2*(j-1)+1
		i2 <- 2*j
		vec.perm[i1:i2] <- sample(vec.by.rows[i1:i2],2)
		}
	mat = matrix(vec.perm, n1, 2, byrow=TRUE)
	res.perm = t.test(mat[,1], mat[,2], paired=TRUE, alternative=tail)
	t.perm = res.perm$statistic

	if(tail == "two.sided") if( abs(t.perm) >= abs(t.ref) ) nPGE <- nPGE+1
	if(tail == "less")      if(t.perm <= t.ref) nPGE <- nPGE+1
	if(tail == "greater")   if(t.perm >= t.ref) nPGE <- nPGE+1
	}

# Compute and print the permutational p-value
P <- nPGE/(nperm+1)
if(!silent) cat('pvalue (',nperm,'permutations ):',formatC(P,digits=5,width=7,format="f"),'\n')
if(!silent) cat('Alternative hypothesis:',tail,'\n','\n')
if(!silent) cat('Be sure the two vectors are properly sorted to one and other','\n','\n')
#
return(list(t.ref=t.ref, p.param=res$p.value, p.perm=P, nperm=nperm))
}

##########################################################################################################
############################################ intern routine ##############################################
##########################################################################################################

t.stat <- function(n1,n2,vec1,vec2, var.equal=var.equal){
moy1 <- mean(vec1)
moy2 <- mean(vec2)
var1 <- var(vec1)
var2 <- var(vec2)

if(var.equal) {
    df <- n1+n2-2
        v <- 0
        if(n1 > 1) v <- v + (n1-1)*var1
        if(n2 > 1) v <- v + (n2-1)*var2
    v <- v/df
    deno <- sqrt(v*(1/n1+1/n2))
} else {
    stderrx <- sqrt(var1/n1)
    stderry <- sqrt(var2/n2)
    deno <- sqrt(stderrx^2 + stderry^2)
    df <- deno^4/(stderrx^4/(n1-1) + stderry^4/(n2-1))
}
t <- (moy1-moy2) / deno
return(list(moy1=moy1,moy2=moy2,var1=var1,var2=var2,t=t, df=df))
}





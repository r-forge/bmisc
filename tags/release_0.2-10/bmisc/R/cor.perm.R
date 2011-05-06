corr.perm <- function (x,y,...) UseMethod("corr.perm")

corr.perm.default <- function(x,y,nperm=999, method="pearson",use = "pairwise.complete.obs",...){
x <- as.matrix(x)
y <- as.matrix(y)
n <- nrow(x)

cor.par <- cor.test(x,y, method=method)
r <- cor.par$estimate
t <- cor.par$statistic

 meth.name=method
 capped <- grep("^[^A-Z]*$",meth.name , perl = TRUE)
 substr(meth.name[capped], 1, 1) <- toupper(substr(meth.name[capped], 1, 1))


nGT <- 1
for(i in 1:nperm)
   {
   y.perm  <-  sample(y,n)
   cor.perm <- cor.test(x,y.perm,method=method,use = "pairwise.complete.obs",...)
   t.perm <- cor.perm$statistic
   if( abs(t.perm) >= abs(t) ) nGT <- nGT+1
   }
P <- nGT/(nperm+1)
res=c(c('\n', meth.name,' correlation (two-tailed test)','\n\n'),
    c('r =',r,'\n'),
    c('t =',t,'\n'),
    c('df =',cor.par$parameter,'\n'),
    c('95% C.I. : (','inf =',cor.par$conf.int[1],' | ','sup =',cor.par$conf.int[2],')','\n'),
    c('p.value(parametric)',ifelse(cor.par$p.value < 0.0001, "< 0.0001", paste('= ',format(cor.par$p.value,scientific=FALSE,digits = 6))),'\n'),
    c('p.value(',nperm,'permutations)',ifelse(P < 0.0001, "< 0.0001", format(P,scientific=FALSE,digits = 6)),'\n','\n'))


cat(res)
don=(list(Correlation=r, t.stat=t, No.perm=nperm, P.perm=P, P.para=cor.par$p.value,inf=cor.par$conf.int[1],
          sup=cor.par$conf.int[2], df=cor.par$parameter))
          



}


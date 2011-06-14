lev <- function (y,data=NULL,...) {
    if(!is.null(data)) {attach(data)}
    UseMethod("lev")

    if(!is.null(data)) {detach(data)}

}

lev.default <-  function (y,  group, data=NULL,  trim.alpha = 0.1, type="abs", form=NULL, ...)
{
    try(if(!is.null(data)) {detach(data)}, silent=TRUE)
    try(if(!is.null(data)) {attach(data)}, silent=TRUE)
    if (!is.numeric(y))
        stop(deparse(substitute(y)), " is not a numeric variable")
    call=match.call()
    DNAME = paste(deparse(substitute(y)), "~",deparse(substitute(group)))
    DATA = deparse(substitute(data))

    if ( trim.alpha >= 0.5) {
        stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean option")
        }

    test=NULL
    test=deparse(substitute(group))
    test=substr(test[1],1,1)
    if(!is.null(test) & length(test)==0 ){
          stop(deparse(substitute(group)), " should be the concatenation of the factors levels. Use 'paste()' or 'interaction()'.")
        }

    mod=paste("\nModel: ",deparse(form),"\n", sep="")
    dmod=paste("\nModel: ",deparse(substitute(y))," ~ ", deparse(substitute(group)), sep="")


  
    group <- as.factor(group)

    data <- structure(list(data.name = DATA), class = "htest")

        means <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], mean)
        switch(type,
          abs = resp.mean <- abs(y - means[group]),
          sq  = resp.mean <- (y - means[group])^2)
        statistic = Anova(lm(resp.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
        METHOD = "Classical Levene's test based on the absolute deviations from the mean."
        p.value = Anova(lm(resp.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
        STATISTIC = statistic
        names(STATISTIC) = "F.value"
        Meval <- structure(list(statistic = STATISTIC, p.value = p.value,
        method = METHOD), class = "htest")


        meds <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], median)
        switch(type,
          abs = resp.med <- abs(y - meds[group]),
          sq  = resp.med <- (y - meds[group])^2)
        statistic = Anova(lm(resp.med ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
        METHOD = "Modified Robust Brown-Forsythe Levene-type test based on the absolute deviations from the median. For non-normal uncentered leptokurtic residuals."
        p.value = Anova(lm(resp.med ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
        STATISTIC = statistic
        names(STATISTIC) = "F.value"
        Medval <- structure(list(statistic = STATISTIC, p.value = p.value,
        method = METHOD), class = "htest")


        trimmed.mean <- function(y) mean(y, trim = trim.alpha)
        trim.means <- tapply(y[is.na(y)==FALSE], group[is.na(y)==FALSE], trimmed.mean)
        switch(type,
          abs = resp.trim.mean <- abs(y - trim.means[group]),
          sq  = resp.trim.mean <- (y - trim.means[group])^2)

        statistic = Anova(lm(resp.trim.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
        METHOD = "Modified Robust Levene-type test based on the absolute deviations from the trimmed mean. It eliminates outliers."
        p.value = Anova(lm(resp.trim.mean ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
        STATISTIC = statistic
        names(STATISTIC) = "F.value"
        Tmeanval <- structure(list(statistic = STATISTIC, p.value = p.value,
        method = METHOD), class = "htest")

       
        grouppn <- tapply(y,group,n)
        meds <- tapply(y, group, median)        
        num=ifelse(grouppn[group]-2 > 0, ( (grouppn[group]-1.5)*grouppn[group]*((y-means[group])**2) - 0.5*tapply((y-means[group])**2,group,sum)[group] )    ,NA)
        den= (grouppn[group]-1)*(grouppn[group]-2)
        r=num/den
        statistic = Anova(lm(r ~ group,contrasts=list(group=contr.sum)), type="III")[2, 3]
        METHOD = "Modified Robust O'Brien Levene-type test based on scores. For non-normal centered meso or platykurtic residuals."
        p.value = Anova(lm(r ~ group,contrasts=list(group=contr.sum)), type="III")[2, 4]
        STATISTIC = statistic
        names(STATISTIC) = "F.value"
        Oval <- structure(list(statistic = STATISTIC, p.value = p.value,
        method = METHOD), class = "htest")
        
        STATISTIC=fligner.test(x=y,g=group)[[1]]
        names(STATISTIC)="Chi-Squared"
        p.value =fligner.test(x=y,g=group)[[3]]
        METHOD = "Fligner-Killeen test of homogeneity of variances"
        Fl=structure(list(statistic = STATISTIC, p.value = p.value, method = METHOD), class = "htest")

   try(if(!is.null(data)) {detach(data)} ,silent=TRUE)

PVAL = c(Meval$p.value, Tmeanval$p.value, Medval$p.value,Oval$p.value, Fl$p.value)
names(PVAL) = c(
        "Levene                  ",
        "Levene on trimed mean   ",
        "Brown.Forsythe          ",
        "OBrien                  ",
        "Fligner-Killeen         ")

STATISTIC = c(Meval$statistic, Tmeanval$statistic, Medval$statistic,Oval$statistic,Fl$statistic)
names(STATISTIC) = c(
    "Levene                  ",
    "Levene on trimed mean   ",
    "Brown.Forsythe          ",
    "OBrien                  ",
    "Fligner-Killeen         ")


RVAL = list(
        formula=call,
        statistic = STATISTIC,
        method = "Levene-type tests",
        p.value = PVAL,
        data.name = DNAME)
class(RVAL) = "list"

if(!is.null(form)) cat(mod)
if(is.null(form)) cat(dmod)


print(new("fHTEST",
        data = list(DNAME),
        test = RVAL,
        title = RVAL$method))





}

lev.formula <- function(y, data=NULL, ...) {

  try(if(!is.null(data)) {detach(data)}, silent=TRUE)
  try(if(!is.null(data)) {attach(data)}, silent=TRUE)
  if("y" %in% ls()==FALSE & (is.null(data) | "data" %in% ls() ==FALSE)) {stop("data hasn't been defined and needs to be")}

  form <- y

  data <- model.frame(form)
  if (any(sapply(2:dim(data)[2], function(j) is.numeric(data[[j]])))) stop("Levene's test is not appropriate with quantitative explanatory variables.")
  y <- data[,1]
  if(dim(data)[2]==2) group <- data[,2]
    else {
      if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
      group <- interaction(data[,2:dim(data)[2]])
  }
  try(if(!is.null(data)) {detach(data)}, silent=TRUE)
 lev.default(y=y,group=group, trim.alpha = 0.1, type="abs",form=form)
}

lev.lm <- function(y, ...) {

  lev.formula(formula(y), data=model.frame(y))

}

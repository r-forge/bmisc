pair.diff <- function (y, group,...) {

   if (!is.null(data)) {attach(data)}

   if(class(y)=="formula" & is.null(data)) {
        stop("'data' should be indicated when using a formula.")
    }
    
  UseMethod("pair.diff")

  try(if(!is.null(data)) {detach(data)}, silent=T)


}


pair.diff.default <- function(y, group, data=NULL,...){

    DNAME <- paste(deparse(substitute(y)), "and", deparse(substitute(group)))
    group <- factor(group)

        METHOD <- "Estimated diffs (Y - Y1)"
        xbar <- tapply(y, group, mean, na.rm = TRUE)
        s <- tapply(y, group, sd, na.rm = TRUE)
        n <- tapply(!is.na(y), group, sum)


        estimate.m <- function(i, j) {
            dif <- xbar[i] - xbar[j]
        }
        estimate.se <- function(i, j) {
            se.dif <- sqrt(((s[i])^2)/n[i] + ((s[j])^2)/n[j])
        }

     try(if(!is.null(data)) {detach(data)}, silent=T)
     try(if(!is.null(data)) {detach(data)}, silent=T)
    mean <- .pair.table(estimate.m, levels(group))
    se <- .pair.table(estimate.se, levels(group))
    ans <- list(diff.m = mean, diff.se = se)
    ans
}


pair.diff.formula = function(y,data=NULL,  ... ) {

  try(if(!is.null(data)) {detach(data)}, silent=T)
  try(if(!is.null(data)) {attach(data)}, silent=T)
  form=y
  dat=model.frame(form)
  group <- interaction(dat[,2:dim(dat)[2]], sep=" ", drop=T)

  y=dat[,1]

  try(if(!is.null(data)) {detach(data)}, silent=T)

  pair.diff.default(y=y,group=group)

}

pair.diff.lm <- function(y, ...) {
  dat=model.frame(y)
  y=formula(y)
  pair.diff.formula(y=y,data=dat)

}




.pair.table <-function(compare.levels, level.names)
{
    ix <- seq_along(level.names)
    names(ix) <- level.names
    pp <- outer(ix[-1L], ix[-length(ix)],function(ivec, jvec)
          sapply(seq_along(ivec), function(k) {
              i<-ivec[k]
              j<-jvec[k]
              if(i>j) compare.levels(i, j) else NA
          }))

    pp
}
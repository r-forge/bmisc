n <- function (x) UseMethod("n")

n.default <- function(x){
    sum(!is.na(x))
 }

 

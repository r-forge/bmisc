sort.vdf <- function (x,by,...) UseMethod("sort.vdf")


sort.vdf.default <- function (x, by, increasing = TRUE){
    
    if (is.data.frame(x)) {
        if (by[[1]] != "~")
            stop("Argument 'by' must be a one-sided formula.")
        formc <- as.character(by[2])
        formc <- gsub(" ", "", formc)
        if (!is.element(substring(formc, 1, 1), c("+", "-")))
            formc <- paste("+", formc, sep = "")
        vars <- unlist(strsplit(formc, "[\\+\\-]"))
        vars <- vars[vars != ""]
        calllist <- list()
        pos <- 1
        for (i in 1:length(vars)) {
            varsign <- substring(formc, pos, pos)
            pos <- pos + 1 + nchar(vars[i])
            if (is.factor(x[, vars[i]])) {
                if (varsign == "-") {
                  calllist[[i]] <- -rank(x[, vars[i]])
                }
                else {
                  calllist[[i]] <- rank(x[, vars[i]])
                }
            }
            else {
                if (varsign == "-") {
                  calllist[[i]] <- -x[, vars[i]]
                }
                else {
                  calllist[[i]] <- x[, vars[i]]
                }
            }
        }
        return(x[do.call("order", calllist), ])
    }
    else {
        decreasing = increasing == FALSE
        sort(x, decreasing =decreasing)
    }
}
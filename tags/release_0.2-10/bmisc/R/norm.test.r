# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.


# Copyrights (C)
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files

norm.test <- function (x,...) UseMethod("norm.test")

norm.test.default <-  function(x, title = NULL, type="mc", comment=T ){   

   
    # Data Set Name:
    
    DNAME <- paste(deparse(substitute(x), 500), collapse="\n")
    
    # Convert Type:
    if (class(x) == "fREG") x = residuals(x)
    x = as.vector(x)
    
    # Call:
    call = match.call()
    
    # Tests:
    ans = NA
    lill = .lil.test(x)
    shaf = .sf.test(x)
    shap = .shap.test(x)
    skew = .skew.test(x, type=type)
    kurt = .kurt.test(x)
    test = .dp.test(x)

    test$data.name = DNAME
    PVAL = c(lill$p.value,shaf$p.value,
            shap$p.value, skew$p.value, kurt$p.value,test$p.value)
        names(PVAL) = c(
        "Lilliefor               ",
        "Shapiro-Francia         ",
        "Shapiro-Wilk            ",
        "Agostino Skewness       ",
        "Anscombe-Glynn Kurtosis ",
        "D'Agostino Pearson      ")



    STATISTIC = c(lill$statistic,shaf$statistic, shap$statistic, skew$statistic[2], kurt$statistic[2],test$statistic)
  
    names(STATISTIC) = c(
        paste(names(lill$statistic),"    | Lilliefor               "),
        paste(names(shaf$statistic),"    | Shapiro-Francia         "),
        paste(names(shap$statistic),"    | Shapiro-Wilk            "),
        paste(names(skew$statistic[2])," | Agostino Skewness       "),
        paste(names(kurt$statistic[2])," | Anscombe-Glynn Kurtosis "),
        paste(names(test$statistic)," | D'Agostino Pearson      ") )

        RVAL = list(
        statistic = STATISTIC,
        method = "Normality tests",
        p.value = PVAL,
        data.name = DNAME)
    #test$statistic = STATISTIC
    #test$p.value = PVAL
    class(RVAL) = "list"
       
    # Add:
    if (is.null(title)) title = paste("Normality Tests on", DNAME)

        
    # Return Value:

    print(new("fHTEST",
        data = list(x = x), 
        test = RVAL,
        title = as.character(title) ))
        
    if(comment){
        mess= "\n If p_value is greater than 0.05, difference between 
  distribution of values and normal (gaussian) distribution 
  is not statiscally significant (i.e. data is normaly distributed).\n\n"
        cat(mess) 
    }
}





.lil.test <- function (x)
{
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n < 5)
        warning("sample size must be greater than 4. Do not rely on Lilliefors test results.")
    p <- pnorm((x - mean(x))/sd(x))
    Dplus <- max(seq(1:n)/n - p)
    Dminus <- max(p - (seq(1:n) - 1)/n)
    K <- max(Dplus, Dminus)
    if (n <= 100) {
        Kd <- K
        nd <- n
    }
    else {
        Kd <- K * ((n/100)^0.49)
        nd <- 100
    }
    pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
        Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
        1.67997/nd)
    if (pvalue > 0.1) {
        KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
        if (KK <= 0.302) {
            pvalue <- 1
        }
        else if (KK <= 0.5) {
            pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
                KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
        }
        else if (KK <= 0.9) {
            pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
                KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
        }
        else if (KK <= 1.31) {
            pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
                KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
        }
        else {
            pvalue <- 0
        }
    }
    RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
        data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}


.sf.test <- function (x)
{
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if ((n < 5 || n > 5000))
        warning("sample size must be between 5 and 5000")
    y <- qnorm(ppoints(n, a = 3/8))
    W <- cor(x, y)^2
    u <- log(n)
    v <- log(u)
    mu <- -1.2725 + 1.0521 * (v - u)
    sig <- 1.0308 - 0.26758 * (v + 2/u)
    z <- (log(1 - W) - mu)/sig
    pval <- pnorm(z, lower.tail = FALSE)
    RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test",
        data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}




# ------------------------------------------------------------------------------


.shap.test <- function (x)
{   
    # A copy from R:

    # FUNCTION:
    
    # Time Series Name:
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    
    if (n < 3 || n > 5000) {warning("sample size must be between 3 and 5000")
    
        RVAL <- list(statistic = NA, p.value = NA, 
            method = "Shapiro-Wilk normality test", data.name = DNAME)
        class(RVAL) <- "htest"
        return(RVAL)
    }
    rng <- x[n] - x[1]
    if (rng == 0) 
        stop("all 'x' values are identical")
    if (rng < 1e-10) 
        x <- x/rng
    n2 <- n%/%2
    sw <- .C("swilk", init = FALSE, as.single(x), n, n1 = as.integer(n), 
        as.integer(n2), a = single(n2), w = double(1), pw = double(1), 
        ifault = integer(1), PACKAGE = "stats")
    if (sw$ifault && sw$ifault != 7) 
        stop(gettextf("ifault=%d. This should not happen", sw$ifault), 
            domain = NA)
    RVAL <- list(
        statistic = c(W = sw$w), 
        p.value = sw$pw, 
        method = "Shapiro-Wilk normality test", 
        data.name = DNAME)
    class(RVAL) <- "htest"
    
    return(RVAL)
}




# ------------------------------------------------------------------------------
# A function implemented by Diethelm Wuertz anf modified by Benoit Bruneau
# ------------------------------------------------------------------------------

.skew.test <- function(x, type="mc")
{   
    # Internal Function for Agostino Normality Test:


    # FUNCTION:
    
    DNAME = deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n = length(x)
    if (n < 16) warning("Sample size must be at least 16. Do not rely on the skewness test results.")

    b1= function (x){
      meanX = mean(x)
      s =  sqrt(mean((x-(mean(x)))**2))
      s3 = mean((x-meanX)**3)/s**3
      s3
      }

    G1= function (x){
      x <- x - mean(x)
      y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
      s3 <- y * sqrt(n * (n - 1))/(n - 2)
      s3
      }
     
    switch(type,
        mc = s3 <- mc(x) ,
        b1 = s3 <- b1(x) ,
        G1 = s3 <- G1(x) )

    
    #x <- x - mean(x)
#    y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
#    s3 <- y * sqrt(n * (n - 1))/(n - 2)
    options(warn=-1)
    SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
    U3 = s3/SD3
    b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
    W2 = sqrt(2*(b-1))-1
    delta = 1/sqrt(log10(sqrt(W2)))
    a = sqrt(2/(W2-1))
    Z3 = delta*log10((U3/a)+sqrt((U3/a)**2+1))
    pZ3 = 2*(1-pnorm(abs(Z3),0,1))
    names(Z3) = "z.b1"
    
    # Result:
    RVAL = list(
        statistic =  c(skew = s3, Z3),
        p.value = pZ3,
        alternative = "skewness is not equal to 0",
        method = paste ("D'Agostino Skewness Test: Skewness estimated by ", type, sep="") ,
        data.name = DNAME)
    options(warn=0)    
    # Return Value:
    class(RVAL) = "htest"
    RVAL
}


# ------------------------------------------------------------------------------


.kurt.test <-  function(x)
{   
    # Internal Function for Agostino Normality Test:
    
    # FUNCTION:
    
    DNAME = deparse(substitute(x))
    
    x <- sort(x[complete.cases(x)])
    n = length(x)
    
    if (n < 16) warning("Sample size must be at least 16. Do not rely on the kurtosis test results.")
    
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a4 = mean((x-meanX)**4)/s**4
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U4 = (a4-3+6/(n+1))/SD4
    B = (6*(n**2-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B)))
    jm = sqrt(2/(9*A))
    pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
    Z4 = (1-2/(9*A)-pos)/jm
    pZ4 = 2*(1-pnorm(abs(Z4),0,1))
    names(Z4) = "z.b2"
    
    # Result:
    RVAL = list(
        statistic = c(kurt = a4, Z4),
        p.value = pZ4,
        alternative = "kurtosis is not equal to 3",
        method = "Anscombe-Glynn Kurtosis Test",
        data.name = DNAME)
        
    # Return Value:
    class(RVAL) = "htest"
    RVAL
}


# ------------------------------------------------------------------------------


.dp.test <- function(x)
{   
    # Internal Function for Agostino Normality Test:

    # FUNCTION:
    
    DNAME = deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n = length(x)
    if (n < 16) warning("sample size must be at least 16. Do not rely on Agostino-Pearson test results.")
    options(warn=-1) 
    k=try(.kurt.test(x), silent=TRUE)
    s=try(.skew.test(x), silent=TRUE)
    options(warn=0) 
    Z3=s$statistic[2]
    Z4=k$statistic[2]
    omni = Z3**2+Z4**2
    pomni = 1-pchisq(omni,2)
    names(omni) = "Chi2"
    
    # Result:
    RVAL = list(
        statistic = omni,
        method = "Agostino Omnibus Normality Test",
        p.value = pomni, 
        data.name = DNAME)
    
    # Return Value:
    class(RVAL) = "htest"
    RVAL
}


# ------------------------------------------------------------------------------






setClass("fHTEST",
    representation(
        data = "list",
        test = "list",
        title = "character")
)


setMethod("show", "fHTEST",
          function(object)
{
    # A function implemented by Diethelm Wuertz

    # Source:
    #   This function copies code from base:print.htest

    # FUNCTION:

    # Unlike print the argument for show is 'object'.
    x = object

    # Title:
    cat("\nTitle:\n ", x@title, "\n", sep = "")



    # Data Name:
    # cat("\nData Name:\n", ans@data.name, "\n", sep = "")

    # Test Results:
    test = x@test
    cat("\nTest Results:\n", sep = "")

    # Tests from tseries package:

    # Parameter:
    if (!is.null(test$parameter)) {
        parameter = test$parameter
        Names = names(parameter)
        cat("  PARAMETER:\n")
        for ( i in 1: length(Names) )
            cat(paste("    ", names(parameter[i]), ": ",
                formatC(parameter[i], digits=4,width=10,format="f"), "\n", sep = "") )
    }

    # Sample Estimates:
    if (!is.null(test$estimate)) {
        estimate = test$estimate
        Names = names(estimate)
        cat("  SAMPLE ESTIMATES:\n")
        for (i in 1:length(Names)) {
            cat(paste("    ", Names[i], ": ",
                formatC(estimate[i], digits=4,width=10,format="f"), "\n", sep = "" ) )
        }
    }

    # Statistic:
    if (!is.null(test$statistic)) {
        statistic = test$statistic
        Names = names(statistic)
        cat("  STATISTIC:\n")
        for (i in 1:length(Names)) {
            if (!is.na(statistic[i])) {
                cat(paste("    ", Names[i], ": ",
                    formatC(statistic[i], digits=4,width=10,format="f"), "\n", sep = "" ) )
            }
        }
    }

    # P-Value:
    if (!is.null(test$p.value)) {
        pval = test$p.value
        Names = names(pval)
        if (Names[1] == "") space = "" else space = ": "
        cat("  P VALUE:\n")
        for (i in 1:length(Names)) {
            if (!is.na(pval[i])) {
                if (class(version) != "Sversion") {
                    cat(paste("    ", Names[i], space,
                    formatC(pval[i], digits=4,width=10,format="f"), " \n", sep = "" ) )
                } else {
                    cat(paste("    ", Names[i], space,
                    formatC(pval[i], digits=4,width=10,format="f"), " \n", sep = "" ) )
                }
            }
        }
    }

    # Confidence Interval:
    if (!is.null(test$conf.int)) {
        conf = test$conf.int
        # For SPlus compatibility use dimnames istead of colnames!
        colNames = dimnames(conf)[[2]]
        cat("  CONFIDENCE INTERVAL:\n")
        for (i in 1:length(colNames)) {
            cat(paste("    ", colNames[i], ": ",
                formatC(conf[1, i], digits=4,width=10,format="f"), ", ",
                formatC(conf[2, i], digits=4,width=10,format="f"), "\n", sep = "" ) )
        }
    }
    
  

    # More Specific Output Results:
    if (!is.null(test$output)) {
        cat(test$output, fill = FALSE, sep = "\n")
    }



    # Return Value:
    #   invisible()  # made visible by DW
})
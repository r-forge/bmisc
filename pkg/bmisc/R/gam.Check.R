gam.Check <- function (b,...) UseMethod("gam.Check")

gam.Check.default <-function (b,main=c("Normal Q-Q Plot","Resids vs. Linear Pred.","Histogram of Residuals","Response vs. Fitted Values"),
          xlab=c("Theorical Quantiles", "Linear Predictor","Residuals","Fitted Values"),
          ylab= c("Sample Quantiles","Residuals","Frequency","Response"),args.histplot=NULL, text=NULL, ...)
{
    if (b$method %in% c("GCV", "GACV", "UBRE", "REML", "ML",
        "P-ML", "P-REML")) {
        old.par <- par(mfrow = c(2, 2))
        sc.name <- b$method
        qqnorm(residuals(b),main=main[1],xlab=xlab[1], ylab=ylab[1])
        mtext( text[1],...)
        
        plot(b$linear.predictors, residuals(b), main = main[2],
            xlab =xlab[2] , ylab =ylab[2] )
        mtext( text[2],...)
        
        if(is.null(args.histplot)) {
                 histplot(residuals(b), xlab=xlab[3], ylab=ylab[3], main=main[3])
            } else {
                DNAME <- paste(deparse(substitute(dat), 500), collapse="\n")
                args.histplot1 <- list(dat=residuals(b),breaks="Sturges", barc="steelblue",
                                       borc="white", fit.norm=TRUE, lcol="brown",
                                       stat=NULL, stat.lab=c("Mean","Median"),
                                       box=TRUE,grid=FALSE,
                                       rug=TRUE , xlab=xlab[3], ylab=ylab[3], main=main[3])
                
                args.histplot1[names(args.histplot)] <- args.histplot
                diff=which(!(names(args.histplot) %in% names(args.histplot1)))
                if(length(diff>=1)){
                    args.histplot1=c(args.histplot1,args.histplot[diff] )
                }
                
                do.call("histplot", args.histplot1)
            }
        mtext( text[3],...)
        
        plot(fitted(b), b$y, xlab =xlab[4] , ylab = ylab[4],
            main = main[4])
        mtext( text[4],...)
        
        cat("\nMethod:", b$method, "  Optimizer:", b$optimizer)
        if (!is.null(b$outer.info)) {
            if (b$optimizer[2] %in% c("newton", "bfgs")) {
                boi <- b$outer.info
                cat("\n", boi$conv, " after ", boi$iter, " iteration",
                  sep = "")
                if (boi$iter == 1)
                  cat(".")
                else cat("s.")
                cat("\nGradient range [", min(boi$grad), ",",
                  max(boi$grad), "]", sep = "")
                cat("\n(score ", b$gcv.ubre, " & scale ", b$sig2,
                  ").", sep = "")
                ev <- eigen(boi$hess)$values
                if (min(ev) > 0)
                  cat("\nHessian positive definite, ")
                else cat("\n")
                cat("eigenvalue range [", min(ev), ",", max(ev),
                  "].\n", sep = "")
            }
            else {
                cat("\n")
                print(b$outer.info)
            }
        }
        else {
            if (length(b$sp) == 0)
                cat("\nModel required no smoothing parameter selection")
            else {
                cat("\nSmoothing parameter selection converged after",
                  b$mgcv.conv$iter, "iteration")
                if (b$mgcv.conv$iter > 1)
                  cat("s")
                if (!b$mgcv.conv$fully.converged)
                  cat(" by steepest\ndescent step failure.\n")
                else cat(".\n")
                cat("The RMS", sc.name, "score gradiant at convergence was",
                  b$mgcv.conv$rms.grad, ".\n")
                if (b$mgcv.conv$hess.pos.def)
                  cat("The Hessian was positive definite.\n")
                else cat("The Hessian was not positive definite.\n")
                cat("The estimated model rank was ", b$mgcv.conv$rank,
                  " (maximum possible: ", b$mgcv.conv$full.rank,
                  ")\n", sep = "")
            }
        }
        cat("\n")
        par(old.par)
    }
    else plot(b$linear.predictor, residuals(b), xlab = xlab[2],
        ylab = ylab[2], main=main[2])
}


        
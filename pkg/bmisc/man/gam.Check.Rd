\name{gam.Check}

\alias{gam.Check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Some diagnostics for a fitted gam model
}
\description{
Takes a fitted gam object produced by gam() and produces some diagnostic
information about the fitting procedure and results. The default is to produce
 4 residual plots, and some information about the convergence of the smoothness
  selection optimization.
}
\usage{
gam.Check(b,...)
\method{gam.Check}{default}(b,
          main=c("Normal Q-Q Plot","Resids vs. Linear Pred.",
          "Histogram of Residuals","Response vs. Fitted Values"),

          xlab=c("Theorical Quantiles", "Linear Predictor",
          "Residuals","Fitted Values"),

          ylab= c("Sample Quantiles","Residuals","Frequency",
          "Response"),
          
          text=NULL, args.histplot=NULL, ...))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{a fitted gam object as produced by \code{gam()}.}
   \item{main}{a character vector containing the four titles to be used.}
   \item{xlab}{a character vector containing the four x labels to be used.}
   \item{ylab}{a character vector containing the four y labels to be used.}
   \item{text}{a character or \code{\link{expression}} vector specifying the text to be written.}
   \item{args.histplot}{\code{\link{list}} of additional arguments to pass to \code{\link{histplot}()}}
   \item{...}{additional text and graphical parameters (see \code{\link{par}}, \code{\link{mtext}})}
}
\details{
This function plots 4 standard diagnostic plots, and some other convergence
diagnostics. Usually the 4 plots are various residual plots. The printed
information relates to the optimization used to select smoothing parameters.
For the default optimization methods the information is summarized in a readable
way, but for other optimization methods, whatever is returned by way of convergence
diagnostics is simply printed.

This is a modified version of \code{\link{gam.check}} from \code{mgcv-package} so that
main titles, x labels and y labels can be customized.
}

\references{
Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC Press.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
library(mgcv)
set.seed(0)
dat <- gamSim(1,n=200)
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b,pages=1)


gam.check(b)                                              

gam.check(b, main=c("A","B","C","D"))                               



}


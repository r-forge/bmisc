\name{lev}

\alias{lev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Levene type tests
}
\description{
Testing for homoscedasticity after an ANOVA.
}
\usage{
lev(y, ...)
\method{lev}{lm}(y, trim.alpha=0.1, type=c("abs","sq"))
\method{lev}{formula}(y, data, trim.alpha=0.1, type=c("abs","sq"))
\method{lev}{default}(y, group, data=NULL, trim.alpha=0.1, type=c("abs","sq"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{y}{response variable for the \code{default} method, \code{lm}
      class object for the \code{lm} method or \code{formula}
      class object for the \code{formula} methode. If \code{y}
      is a linear-model object or a formula, the variables on the right-hand-side
      of the model must all be factors and must be completely crossed.}
   \item{group}{for the default method, factor (concatenated factor when multiple factors).}
   \item{data}{\code{\link{data.frame}} containing the dependant variable and the factor(s).}
   \item{trim.alpha}{Alpha level (percentiles) trimming the data on which the mean will be evaluated.}
   \item{type}{Type of transformation made on the residuals. Either "abs" for absolute values or "sq" for squared values}
}
\details{
When using the \code{lm} method, \code{data} doesn't need to be defined. When using the
 \code{formula} or \code{default} methods, \code{data} can be defined if the data used
 is in a \code{\link{data.frame}}.

When group is manually defined in the default method, use \code{paste(x,y,z)} or\\*
\code{interaction(x,y,z)}form where\code{"x"}, \code{"y"} and \code{"z"}
are the factors. 

O'Brien's (1981) performs test for equality of variances within each group:
based on transforming each observation in relation to its group variance
and its deviation from its group mean; and performing an ANOVA on these
transformed scores (for which the group mean is equal to the variance of the
original observations). The procedure is recognised to be robust against
violations of normality (unlike F-max).

More to come.
}
\value{
 \code{lev} returns an object of \code{class(S4)} \code{"lev"}.
 
 An object of class \code{"ypr"} has the the following slots:
 
\item{statistics}{statistics for each test.}
\item{p.value}{p-value for each test.}
\item{residuals}{residuals for each test (r-scores for O'Briens test.}


}
\references{
Brown, Morton B. and Alan B. Forsythe  (1974). Robust Tests for Equality of Variances, J. Am. Statistical Assoc., 69, 364–367.

Fligner, Michael A. and Timothy J. Killeen (1976). Distribution-free two sample tests for scale. J. Am. Statistical Assoc., 71: 210-213.

Levene, Howard (1960). "Robust tests for equality of variances". In Ingram Olkin, Harold Hotelling, et alia. Stanford University Press. pp. 278–292.

O’Brien, R.G. (1981). A simple test for variance effects in experimental designs. Psychological Bulletin, 89, 570–574.

Conover, William J., Johnson, Mark E., and Myrle M. Johnson (1981). A comparative study of tests for homogeneity of variances, with applications to the outer continental shelf bidding data. Technometrics 23, 351–361. 
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ligner.test}}
}
\examples{
z=data.frame( yy=c(rep("c",50),rep("d",50)),
              x=c(rnorm(50),rnorm(50,10)),
              s=rep(c(rep("a",25),rep("b",25)),2),
              qq=rep(c(rep("w",10),rep("t",10)),5))

mod=lm(x~yy*qq*s, data=z)
formula= x~yy*qq*s

lev(y=x, group= paste(yy,qq,s), data=z, type="abs")
lev(y=x, group= paste(yy,qq,s), data=z, type="sq")

lev(y=x, group= interaction(yy,qq,s), data=z)

lev(y=formula, data=z)
lev(mod)

}


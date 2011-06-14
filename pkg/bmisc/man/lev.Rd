\name{lev}

\alias{lev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Levene type tests
}
\description{
Tests heteroscedasticity after an Anova
}
\usage{
lev(y, ...)
\method{lev}{formula}(y, data=NULL,  ...)
\method{lev}{lm}(y, ...)
\method{lev}{default}(y, group, data=NULL , trim.alpha = 0.1, type="abs",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{y}{response variable for the \code{default} method, \code{lm}
      class object for the \code{lm} method or \code{formula}
      class object for the \code{formula} methode. If \code{y}
      is a linear-model object or a formula, the variables on the right-hand-side
      of the model must all be factors and must be completely crossed. See details.}
   \item{group}{for the default method, factor (concatenated factor when multiple factors). See details.}
   \item{data}{\code{\link{data.frame}} where the dependant variable and the factor(s) are}
   \item{trim.alpha}{Alpha level (percentiles) trimming the data on which the mean will be evaluated}
   \item{type}{Type of transformation made on the residuals. Either "abs" for absolute values or "sq" for sqared values}
   \item{...}{arguments to be passed down, e.g., \code{data} for the
     \code{formula} method or other options such as \code{type} and \code{trim.alpha}.}
}
\details{
When using the \code{lm} method, \code{data} doesn't need to be defined. When using the
 \code{formula} or \code{default} methods, \code{data} can be defined if the data used
 is in a \code{\link{data.frame}}.

When group is manually defined in the default method, use \code{paste(x,y,z)} or\\*
\code{interaction(x,y,z)}form where\code{"x"}, \code{"y"} and \code{"z"}
are the factors. There is no restrictions on the number of factors.

O'Brien's (1981) performs test for equality of variances within each group:
based on transforming each observation in relation to its group variance
and its deviation from its group mean; and performing an ANOVA on these
transformed scores (for which the group mean is equal to the variance of the
original observations). The procedure is recognised to be robust against
violations of normality (unlike F-max).


}
\value{

\item{Model}{The model}
\item{Levene}{Results for Levene's test}
\item{LeveneTrimMean}{Results for Levene's test on the trimmed mean}
\item{Brown.Forsythe}{Results for Brown-Forsythe's test}
\item{OBrien}{Results for O'Brien's test}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{leveneTest}} from \code{{car}}
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


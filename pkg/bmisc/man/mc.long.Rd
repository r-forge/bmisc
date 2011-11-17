\name{mc.long}

\alias{mc.long}

\title{Pairwise t tests in long format}

\description{
Calculate pairwise T tests between group levels with corrections for multiple testing presented in long format
}
\usage{
mc.long(y, ...)
\method{mc.long}{formula}( y, data=NULL, subset, ...)
\method{mc.long}{lm}( y, ...)
\method{mc.long}{default}(y, group, data=NULL, p.adjust.method="holm",
        column=NULL, digits=NULL, subset, silent=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{response variable for the default method, or \code{lm} or
  \code{\link{formula}} object. If \code{y} is a linear-model object or a formula,
  the variables on the right-hand-side of the model must all be factors and
  must be completely crossed.}
  \item{group}{for the default method, factor (concatenated factor when multiple factors). See details.}
  \item{data}{\code{\link{data.frame}} where the dependant variable and the factor(s) are}
  \item{p.adjust.method}{method for adjusting p values. Default is Holm's method. (see \code{\link{P.adjust}}) }
  \item{column}{new names for the factor(s); this is optional }
  \item{digits}{controls the number of digits for the presented results presented}
  \item{subset}{logical expression indicating elements to keep: missing values are taken as false.}
  \item{silent}{a logical variable indicating whether to indicate the general \code{warning} \code{(FALSE)} or not \code{(TRUE)}.}
  \item{\dots}{additional arguments to pass to \code{\link{p.adjust}}, \code{\link{pairwise.t.test}} and/or \code{\link{t.test}}. }
}
\details{
When making multiple t tests for all combinations, the \code{n} option of
\code{\link{P.adjust}} can be used to identify the number of comparisons that are actually used.
This is only to simplify the uses p values corrections on the full output
matrix when only some of the comparisons are meaningfull or chosen for hypothesis testing.


When \code{group} is manually defined, use \code{\link{paste}(x,y,z)} or
\code{\link{interaction}(x,y,z)}form; \code{"x"}, \code{"y"} and \code{"z"} are the factors. There is no restrictions on the number of factors.


}
\value{
  Object of class \code{"data.frame"} containing the results.

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
\code{\link{p.adjust}}, \code{\link{pairwise.t.test}}, \code{\link{pair.diff}}, \code{\link{DTK.test}}, \code{\link{TukeyHSD}} and \code{\link{glht}}
}
\examples{

z=data.frame( yy=c(rep("c",50),rep("d",50)),
              x=c(rnorm(50),rnorm(50,10)),
              s=rep(c(rep("a",25),rep("b",25)),2),
              qq=rep(c(rep("w",10),rep("t",10)),5))

mod=lm(x~yy*qq*s, data=z)
formula= x~yy*qq*s

mc.long(y=x, group= paste(yy,qq,s), data=z)
mc.long(y=x, group= paste(yy,qq,s), data=z, p.adjust.method="sidak")
mc.long(y=x, group= paste(yy,qq,s), data=z, p.adjust.method="sidak", n=15)
mc.long(y=x, group= interaction(yy,qq,s), data=z)

mc.long(y=formula, data=z)

mc.long(mod)

res <- mc.long(mod)   #### results are put in "res" object.

}


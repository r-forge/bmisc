\name{pair.diff}

\alias{pair.diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean differences matrix and their associated standard Errors
}
\description{
Creates two lower triangle matrix: The mean differences and their standard error.
}
\usage{
pair.diff(y, ...)
\method{pair.diff}{formula}(y, data=NULL ...)
\method{pair.diff}{lm}( y, ...)
\method{pair.diff}{default}( y, group, data=NULL, ...)
}

\arguments{
\item{y}{response variable for the default method, or \code{lm} or
  \code{\link{formula}} object. If \code{y} is a linear-model object or a formula,
  the variables on the right-hand-side of the model must all be factors and
  must be completely crossed.}
  \item{group}{for the default method, factor (concatenated factor when multiple factors). See details.}
  \item{data}{\code{data.frame} where the dependant variable and the factor(s) are.}
  \item{\dots}{ additional arguments to pass to \code{\link{mean}} and/or \code{\link{sd}}. }
}
\details{
When group is manually defined, use \code{paste(x,y,z)} or \code{interaction(x,y,z)}form where\code{"x"}, \code{"y"} and \code{"z"} are the factors. There is no restrictions on the number of factors.

This function can be usefull with \code{\link{pairwise.t.test}} since the matrix created are of the same format.
}
\value{
Object of class \code{"list"} containing two matrices:

  \item{diff.m  }{Mean differences half matrix }
  \item{diff.se }{Standard error associated with the mean differences half matrix}

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
Is included in \code{\link{mc.long}} for the long format of the results.
}
\examples{

z=data.frame( yy=c(rep("c",50),rep("d",50)),
              x=c(rnorm(50),rnorm(50,10)),
              s=rep(c(rep("a",25),rep("b",25)),2),
              qq=rep(c(rep("w",10),rep("t",10)),5))

mod=lm(x~yy*qq*s, data=z)
y= x~yy*qq*s

pair.diff(y=x, group= paste(yy,qq,s), data=z)
pair.diff(y=x, group= interaction(yy,qq,s), data=z)
pair.diff(y=y, data=z)
pair.diff(mod)
}


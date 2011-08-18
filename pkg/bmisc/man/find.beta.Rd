\name{find.beta}
\alias{find.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Logistic curve parameter estimates
}
\description{
Finds the parameters of a logistic curve for given inflection points.
}
\usage{
find.beta(beta=0.5, minv, maxv, prop=0.01)
}

\arguments{
  \item{beta}{stating value of beta. Default is \code{0.5}.}
  \item{minv}{the minimum value on the abscissa is the first inflection point.}
  \item{maxv}{the maximum value on the abscissa is the second inflection point.}
  \item{prop}{the proportion of the instantaneous slope at 50\% probability that should be used to define the position of the inflection points of the curve. Default is \code{0.01}.}


}
\details{
A logistic curve is defined by:

\tabular{c}{
\eqn{y=1/(1+e^{-(\alpha+\beta x)}) < == > 1/(1+e^{-\beta(x-x_{50})})}
}
Depending on the sign of \eqn{\beta}, the curve will be negative or positive.
}
\value{
  \code{find.beta()} returns a \code{\link{data.frame}} with the following columns:

  \item{beta}{the estimated \eqn{\beta} for the given inflection points.}
  \item{alpha}{the estimated \eqn{\alpha} for the given inflection points.}
  \item{x50}{the value of \code{x} when \code{y} is \code{0.5} (\eqn{x_{50}}).}
  \item{angle.x50}{the angle of the instantaneous slope at \eqn{x_{50}}.}
  \item{min}{the value of the first inflection point.}
  \item{max}{the value of the second inflection point.}
  \item{angle.infl}{the angle of the instantaneous slope at the inflection points.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benoit Bruneau
}
\note{


}



\seealso{
\code{\link{deriv}}
}
\examples{
res1=find.beta(beta=0.1,minv=1000,maxv=1700, prop=0.01)
res2=find.beta(beta=0.1,minv=500,maxv=1700, prop=0.01)
par(mfrow=c(2,1))

xlim=c(0,res1$max+((res1$max-0)*0.2))

curve(1/(1+exp(-res1$beta*(x-res1$x50))), xlim=xlim, ylab="Probability",lwd=2)
abline(v=c(res1$max,res1$min,res1$x50), col=c("red","red","blue"))
lines(x=c(-500,res1$x50),y=c(0.5,0.5), lty=2, col=gray(0.4))
text(x=0,y=0.5,labels="x50", pos=3, col=gray(0.4))

curve(1/(1+exp(-res2$beta*(x-res2$x50))), xlim=xlim,ylab="Probability",lwd=2)
abline(v=c(res2$max,res2$min,res2$x50),, col=c("red","red","blue"))
lines(x=c(-500,res2$x50),y=c(0.5,0.5), lty=2, col=gray(0.4))
text(x=0,y=0.5,labels="x50", pos=3, col=gray(0.4))
}



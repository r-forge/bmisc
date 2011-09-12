\name{selectivity}
\alias{selectivity}
\alias{const.sel}
\alias{full.sel}
\alias{plat.full.sel}
\alias{ramp.sel}
\alias{plat.ramp.sel}
\alias{logit.sel}
\alias{plat.logit.sel}
\alias{mod.logit.sel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Selectivity functions
}
\description{
These selectivity functions are called by \code{\link{ypr}()}. They estimate probabilities [0,1] for a given functional shape and a given number of inflection points .
}
\usage{
const.sel(x)
full.sel(x, infl1, pos=TRUE)
plat.full.sel(x, infl1, infl2, pos=TRUE)
ramp.sel(x, infl1, infl2, pos=TRUE)
plat.ramp.sel(x, infl1, infl2, infl3, infl4, pos=TRUE)
logit.sel(x, infl1, infl2, pos=TRUE, ...)
plat.logit.sel(x, infl1, infl2, pos=TRUE, ...)
mod.logit.sel(x, alpha, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector for which probabilities are to be estimated.
}
  \item{infl1 to infl4}{numeric value of the inflection point(s).
}
  \item{pos}{logical. Indicates if the trend at the beginning is positive  (\code{TRUE}) or negative (\code{FALSE})
}
  \item{\dots}{arguments to be passed to \code{\link{find.beta}()}
}
}
\details{
More to come.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{ypr}}, \code{\link{find.beta}}
}
\examples{

library(bmisc)

x=0:1000

plot(full.sel(infl1=600,x=0:1000,lv=0,uv=1), ylim=c(0,1), type='l', lwd=3)

plot(plat.full.sel(infl1=300, infl2=800,x=x,lv=c(0.4), uv=0.9, neg=F) ~ x, ylim=c(0,1), type='l', lwd=3)

plot(ramp.sel(200,600,x=0:1000, lv=0.1,uv=1, neg=T), ylim=c(0,1), type='l', lwd=3)

plot(plat.ramp.sel(infl1=100,infl2=300,infl3=600,infl4=800,x=0:1000, lv=c(0.3,0.1), uv=c(0.7,0.5), neg=F), ylim=c(0,1), type='l', lwd=3)

plot(logit.sel(infl1=300,infl2=500,x=0:1000, lv=0.5, uv=1, neg=T), ylim=c(0,1), type='l', lwd=3)

plot(plat.logit.sel(infl1=200,infl2=400,infl3=600,infl4=800,x=0:1000, lv=c(0.2,0.8)), ylim=c(0,1), type='l', lwd=3)
}



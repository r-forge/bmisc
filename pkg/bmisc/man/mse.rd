\name{mse}

\alias{mse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean square error
}
\description{
Estimates the mean square error (mse)
}
\usage{
mse(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{an object containing the results of a model. 
}

}
\details{
The mean square error is also known as the unexplained variance or the variance of the residuals.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
z=data.frame( yy=c(rep("c",50),rep("d",50)),
              x=c(rnorm(50),rnorm(50,10)),
              s=rep(c(rep("a",25),rep("b",25)),2),
              qq=rep(c(rep("w",10),rep("t",10)),5))

mod=lm(x~yy*qq*s, data=z)

mse(mod)

}


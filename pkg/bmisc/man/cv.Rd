\name{cv}

\alias{cv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coefficient of Variation (CV)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cv(x, na.rm=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an \R object (vector, matrix,...)
}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds
}
}
\details{
The coefficient of variation (CV) is the ratio of the standard deviation to the mean.
The CV is defined for the absolute value of the mean to ensure it is always positive. 
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
x=rnorm(50)
cv(x)
}


\name{roundup}

\alias{roundup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
roundup
}
\description{
The "conventional" rounding of 5 to the higher value
}
\usage{
roundup(x, numdigits = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector.}
  \item{digits}{integer indicating the number of decimal places to be used.}
}
\details{

Rounds a 5 to the next value. Therefore roundup(2.5) is 3.
This can be usefull when the rounded values are to be presented in a document (eg. table, graph,...).

When rounded values are used in other calculations, \code{\link{round}} should be
used since it follows the IEC 60559 standard.
}
\value{
numeric vector.
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
\code{\link{round}}
}
\examples{
round(2.5)
roundup(2.5)
}



\name{unload}

\alias{unload}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unload packages
}
\description{
Unloads one or multiple packages.
}
\usage{
unload(pack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pack}{Character vector specifying which packages to unload.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
Benoit Bruneau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(mgcv)
search()
unload(mgcv)
search()
}


\name{n}

\alias{n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size (n)
}
\description{
Gives n without NA's
}
\usage{
n(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector (numeric or character)
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
x= rep(c(rnorm(30,20,5),NA),3)
n(x)
}


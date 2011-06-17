\name{sort.vdf}

\alias{sort.vdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort Data Frames and Vectors
}
\description{Single function enabling \code{data.frame} and \code{vector} sorting}
\usage{
sort.vdf(x, by, increasing=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{data.frame} or \code{vector}}
  \item{by}{A one-sided formula using + for ascending and - for descending. Sorting is left to right in the formula. This is for data.frame only.}
  \item{increasing}{logical. Should the sort be increasing \code{(TRUE)} or decreasing \code{(FALSE)}? This is for sorting vectors only.}

}
\details{
See example.
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
Kevin Wright and modified by Benoit Bruneau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x=rnorm(10)
y=runif(30)
z=data.frame(x,y)

sort.vdf(x)                 ### Sort a vector in increasing order
sort.vdf(z)                 ### Gives an error message
sort.vdf(z,by= ~ +x)        ### Sort (z) by a column (+x)
sort.vdf(z,by= ~ +x +y)     ### Sort (z) by two column (+x and then +y)
sort.vdf(z,by= ~ +x -y)     ### Sort (z) by two column (+x and then -y)
}


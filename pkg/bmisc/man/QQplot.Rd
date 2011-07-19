\name{QQplot}

\alias{QQplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QQplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QQplot(dat, quant=TRUE,cex.q=2,norm=T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dat}{one of:
    	\itemize{
      		\item a numeric vector,
      		\item an object of class \code{"norm"} resulting from a
    			call to \code{\link{norm.test}}}
    }
  \item{quant}{logical; T for adding quantiles 75, 50 (median) and 25.}
  \item{cex.q}{numeric vector giving the amount by which plotting symbols should be magnified relative to the default}
  \item{norm}{logical; T adds a line to a normal quantile-quantile plot.}
  \item{...}{additional arguments to be passed (see \code{\link{par}}, \code{\link{qqnorm}}})
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
x=rnorm(50)
QQplot(x)

norm.x=norm.test(x)
QQplot(norm.x)  
}


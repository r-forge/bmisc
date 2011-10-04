\name{rivard}
\alias{rivard}

\title{
Rivard Weights Calculation
}
\description{
This function applies Rivard equations to mid-year weight at age data to adjust values to Jan-1 basis. 
}
\usage{
rivard(pds, pred=FALSE, K=2, plus.gr=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{fsel.type} here~~
}
}
\details{
More to come.  Will be adding interpolation for spawning season.
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
François Grégoire
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x=rnorm(30,800,10)
rivard(data.frame("2000"=x,"2001"=x*1.2, "2002"=x*0.8,"2003"=x*0.5))

}


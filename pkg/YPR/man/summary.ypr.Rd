\name{summary.ypr}
\alias{summary.ypr}

\title{Summarizing the results of YPR models.
}
\description{
Summary for an object of class \code{"ypr"}.
}
\usage{
\S4method{summary}{ypr}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{"ypr"} resulting from a
    call to \code{\link{ypr}}.
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
ypr.mod = ypr.l(fsel.type=list("ramp",650,900), vonB=c(1564.512,0.1205765), 
                l.start=72.53,last.age=25,LW=c(exp( -18.47894),3.043), F.max=2,
                F.incr.YPR=0.01, M=0.2, mat=list('full',900), f.MSP=0.4, riv.calc=F)  

summary(ypr.mod)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


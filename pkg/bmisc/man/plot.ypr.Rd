\name{plot.ypr}
\alias{plot.ypr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Yield per Recruit plot.
}
\description{
Yield per Recruit and Spawning Stock Biomass per Recruit are plotted with standard reference points. 
}
\usage{
 plot.ypr(object, main, ylab.ypr, ylab.ssb, xlab, col.ypr, col.ssb, ref, legend)
}

\arguments{
  \item{object}{an object og \code{class 'ypr'}
}
  \item{main}{main title for the graph
}
  \item{ylab.ypr}{a label for the YPR \code{y} axis
}
  \item{ylab.ssb}{a label for the SSB/R \code{y} axis
}
  \item{xlab}{a label for the YPR \code{x} axis.
}
  \item{col.ypr}{the color of the the color of the YPR line.
}
  \item{col.ssb}{the color of the the color of the SSB/R line.
}
  \item{ref}{logical; if \code{TRUE}, standard reference points are added to the plot.
}
  \item{legend}{logical; if \code{TRUE}, a legend is added in the \code{'topright'} corner of the plot.
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
\code{\link{ypr.l}}
}
\examples{
ypr.mod = ypr.l(fsel.type=list("ramp",650,900), vonB=c(1564.512,0.1205765), 
                l.start=72.53,last.age=25,LW=c(exp( -18.47894),3.043), F.max=2,
                F.incr.YPR=0.01, M=0.2, mat=list('full',900), f.MSP=0.4, riv.calc=F)  

plot(ypr.mod)

}


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
\S4method{plot}{ypr}(object, main, ylab.ypr, ylab.ssb, xlab, 
                     col.ypr, col.ssb, ref, legend)
}

\arguments{
  \item{object}{an object of class \code{ypr} resulting from a
    call to \code{\link{ypr}}
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

\bold{REFERENCE POINTS:}\cr
Reference points used for result output are defined as follow:
\itemize{
    \item \bold{F.zero:}  F level when there is no fishing (F=0).
    \item \bold{F.01:}    F level where the slope of yield curve is 10\% of the slope at \code{F.zero}. 
    \item \bold{F.xx:}    F level where the MSP is at the level defined by \code{f.MSP} option. Default is 40\% (0.4).
    \item \bold{F.max:}   F level where yield is maximum.
}
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
\code{\link{ypr}}
}
\examples{

ypr.mod=ypr(vonB=c(1564.5508, 0.1206),  
		LW=c(0.000000009434 , 3.042974620048),
		l.start=241.86,
		last.age=14,
		age.step=1,
		Fsel.type=list(type="ramp",infl1=650,infl2=900, lv=c(0,0.2)),
		F.max=2, 
		F.incr.YPR=0.0001,
		Msel.type=list(type="ramp", infl1=400, infl2=600, lv=0.5, pos=F), 
		M=0.2, 
		Mat=list(type="ramp",infl1=700, infl2=900),
		f.MSP=0.4) 



plot(ypr.mod)

}


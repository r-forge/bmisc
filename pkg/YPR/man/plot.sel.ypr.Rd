\name{plot.sel.ypr}
\alias{plot.sel.ypr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots of selectivity curves used in an \code{ypr} object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.sel.ypr(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{ypr} object.
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



plot.sel(ypr.mod)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

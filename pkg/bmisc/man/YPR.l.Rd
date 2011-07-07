\name{YPR.l}
\alias{YPR.l}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Length Based Yield Per Recruit 
}
\description{
Length based Yield Per Recruit model is define by fishery selectivity and life history parameters related to length. 


}
\usage{
YPR.l(fsel.type, last.age, vonB, l.start, LW, F.max, F.incr.YPR, M, full.mat, f.MSP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fsel.type}{a list containing the type of fishery selectivity and the values needed for the function related to the type. See 'Details'. 
}
  \item{last.age}{last age to be considered in the model
}
  \item{vonB}{a vector containing \code{c(Linf,K)} from von Bertalanffy grotwh curve. 
}
  \item{l.start}{length at the starting age
}
  \item{LW}{}{a vector containing \code{c(alpha,beta)} from length-weight curve.
}
  \item{F.max}{maximum value of instantaneous rate of fishing mortality (\code{F})
}
  \item{F.incr.YPR}{increment value to be used for YPR calculation
}
  \item{M}{instantaneous rate of natural mortality (\code{M})
}
  \item{full.mat}{fully mature length value
}
  \item{f.MSP}{reference point defined as the fraction of maximum spawning potential

}
}
\details{
More to com.
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

YPR.l(fsel.type=list("ramp",650,900), vonB=c(1564.512,0.1205765), l.start=72.53,last.age=25,
        LW=c(exp( -18.47894),3.043),F.max=2, F.incr.YPR=0.01, M=0.2, full.mat=900, f.MSP=0.4) 

}


\name{ypr.l}
\alias{ypr.l}

\title{
Length Based Yield Per Recruit 
}
\description{
Length based Yield Per Recruit model is define by fishery selectivity and life history parameters related to length. 


}
\usage{
 ypr.l(fsel.type, last.age, age.step, vonB, l.start, LW, F.max, F.incr.YPR, mat, 
         M, f.MSP, riv.calc=FALSE, F.f=0, M.f=0.5)
}

\arguments{
  \item{fsel.type}{a list containing the type of fishery selectivity and the values needed for the function related to the type. See 'Details'. 
}
  \item{last.age}{last age to be considered in the model
}
  \item{age.step}{steps used to generate ages (\code{0:last.age}). Default is \code{1}.
}
  \item{vonB}{a vector containing \code{c(Linf,K)} from von Bertalanffy grotwh curve. 
}
  \item{l.start}{length at the starting age
}
  \item{LW}{}{a vector containing \code{c(alpha,beta)} from length-weight curve.
}
  \item{F.max}{maximum value of instantaneous rate of fishing mortality (\code{F}). Default is \code{2}.
}
  \item{F.incr.YPR}{increment for generating the \code{F} values to be used for YPR calculation. Default is \code{0.0001}.
}
  \item{M}{instantaneous rate of natural mortality (\code{M}). Default is \code{0.2}.
}
  \item{mat}{a list containing the type of maturity definition and the values needed for the function related to the type. See 'Details'.
}
  \item{f.MSP}{reference point defined as the fraction of maximum spawning potential. Default is \code{0.4}.
}
  \item{riv.calc}{a logical value indicating whether to use Rivard weights calculation (\code{TRUE}) or not. Default is \code{FALSE}.
}
  \item{F.f}{fraction of instantaneous rate of fishing mortality (\code{F}) before spawning.
}
  \item{M.f}{fraction of instantaneous rate of natural mortality (\code{M}) before spawning.
}
}
\details{
More to come.
}
\value{
  \code{ypr.l} returns an object of \code{\link{class}(S4)} \code{"ypr"}. The functions \code{summary} and \code{plot} are used to respectively
  obtain a summary and a standard plot of the results.  
  
  An object of class \code{"ypr"} has the the following slots:


  \item{parms}{the list of parameters used in the model.}
  \item{base}{a \code{\link{data.frame}} containing the starting values:
        \itemize{
            \item relative age classes
            \item length at age
            \item weight at age
        }
  }
  \item{refs}{a \code{\link{data.frame}} containing values predicted by the model for the four reference points. See details. }
  \item{YPR}{a \code{\link{data.frame}} containing the results for all partial Fs.}
  

  Note that to have access to each slot of an \code{"ypr"} object, one must use \code{"@"} instead of \code{"$"}.
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

ypr.l(fsel.type=list("ramp",650,900), vonB=c(1564.512,0.1205765), 
      l.start=72.53,last.age=25,LW=c(exp( -18.47894),3.043), F.max=2,
      F.incr.YPR=0.01, M=0.2, mat=list('full',900), f.MSP=0.4, riv.calc=F) 

}


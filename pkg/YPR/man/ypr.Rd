\name{ypr}
\alias{ypr}

\title{
Length Based Yield Per Recruit 
}
\description{
Length based Yield Per Recruit model is define by fishery selectivity and life history parameters related to length. 


}
\usage{
 ypr(LW, vonB, l.start, last.age, age.step=1, Fsel.type, 
       F.max=2,F.incr.YPR=0.0001, Mat,  M=0.2, f.MSP=0.4)%, 
       %F.f=0, M.f=0.5, riv.calc=FALSE)
}

\arguments{
  \item{LW}{one of:
        \itemize{
            \item a vector containing \code{c(\eqn{\alpha},\eqn{\beta})} from length-weight curve. See \sQuote{Details}. 
            \item an object of class \code{"\link{nls}"} in which \eqn{\alpha} and \eqn{\beta} were estimated. See \sQuote{Details}.        
        }
}
  \item{vonB}{one of:
        \itemize{
            \item a vector containing \code{c(Linf,K)} from von Bertalanffy grotwh curve.
            \item an object of class \code{"\link{glm}"} in which eqn{Linf} and eqn{K} were estimated.      
        }
}
  \item{l.start}{length at the starting age
}
  \item{last.age}{last age to be considered in the model
}
  \item{age.step}{steps used to generate ages. Default is \code{1}.
}

  \item{Fsel.type}{fishing selectivity can be defined as one of:
        \itemize{
            \item a list containing the type of fishery selectivity and the values needed for the function related to the type.
            \item an object of class \code{"\link{glm}"} in which \eqn{\alpha} and \eqn{\beta} were estimated by a logistic regression.        
        }
}
  \item{fish.lim}{the minimum legal catch length.
}
  \item{prop.surv}{a function that defines the proportion of fish (< \code{fish.lim}) that will survive after being released back into the water (discarded by-catch).
}
  \item{F.max}{maximum value of instantaneous rate of fishing mortality (\code{F}). Default is 2.
}
  \item{F.incr.YPR}{increment for generating the \code{F} values to be used for YPR calculation. Default is 0.0001.
}
  \item{Mat}{Maturity can be defined by one of:
        \itemize{
            \item a list containing the type of maturity at length definition and the values needed for the function related to the type.
            \item an object of class \code{"\link{glm}"} in which \eqn{\alpha} and \eqn{\beta} were estimated by a logistic regression.       
        }
}
  \item{Msel.type}{natural mortality selectivity can be defined as one of:
        \itemize{
            \item a list containing the type of natural mortality selectivity and the values needed for the function related to the type. 
            \item an object of class \code{"\link{glm}"} in which \eqn{\alpha} and \eqn{\beta} were estimated by a logistic regression.       
        }
}
  \item{M}{instantaneous rate of natural mortality (\code{M}). Default is 0.2.
}
  \item{f.MSP}{reference point defined as the fraction of maximum spawning potential. Default is 0.4.
}
%  \item{F.f}{fraction of instantaneous rate of fishing mortality (\code{F}) before spawning.
%}
%  \item{M.f}{fraction of instantaneous rate of natural mortality (\code{M}) before spawning.
%}
%  \item{riv.calc}{a logical value indicating whether to use Rivard weights calculation (\code{TRUE}) or not. Default is \code{FALSE}.
%}
}
\details{
\bold{LENGTH-WEIGHT RELATIONSHIP:}\cr
Length-Weight relationship can be provided either by indicating \code{c(\eqn{\alpha},\eqn{\beta})} values in a vector or by 
directly using an object of class \code{"\link{nls}"} or \code{"\link{lm}"}. If \eqn{\alpha} and \eqn{\beta} are estimated 
by \code{\link{lm}}, \code{\link{log}(x, base=exp(1))} transformation should be applied to the data prior to fitting the linear model. 
If \eqn{\alpha} and \eqn{\beta} are estimated by \code{\link{nls}}, variables should be named \bold{alpha} and \bold{beta} 
using the following equation:
\tabular{c}{
\eqn{W=\alpha L^\beta}
}
where \eqn{W} is weight, \eqn{L} is length, \eqn{\alpha} is the elevation of the curve, and \eqn{\beta} 
is the steepness of the curve. Both \eqn{\alpha} and \eqn{\beta} are estimated coefficients.\cr\cr


\bold{VON BARTANLANFFY GROWTH EQUATION:}\cr
Von Bartalanffy growth equation parameters can be provided either by indicating \code{c(Linf,K)} values in a vector or by 
directly using an object of class \code{"\link{nls}"}. If an object resulting from \code{\link{nls}} is used, variables 
should be named \bold{Linf} and \bold{K}. As for \bold{\eqn{t0}}, any name may be used since only \eqn{L\infty } and \eqn{K} are 
used in this length-based YPR model because age is considered as relative. The equation used in the \code{\link{nls}} for estimating \eqn{L\infty } 
and \eqn{K} should be the following one:

\tabular{c}{
\eqn{ L_t=L_{\infty }\bigg{(} 1-e^{-K (t-t_0)}\bigg{)} }
}
where \eqn{Lt} is length-at-age \eqn{t}, \eqn{L\infty} is the asymptotic average maximum length, 
\eqn{K} is a growth rate coefficient determinant of how quick the maximum is attained, and \eqn{t0} 
is the hypothetical age at length zero.

As stated above, since this length-based YPR model uses relative age, \eqn{t-t0} becomes a relative age (\eqn{a}). 
The Von Bartalanffy growth equation used in this length-based YPR model is defined as:
\tabular{c}{
\eqn{ L_a=L_{\infty }\bigg{(} 1-e^{-Ka}\bigg{)}+ L_s e^{-Ka} }
}
where \eqn{L_a} is length at a relative age \eqn{a} and \eqn{L_s} is length at relative age zero.\cr\cr


\bold{SELECTIVITY CURVES:}\cr
The \bold{fishery selectivity}, \bold{natural mortality selectivity}, and \bold{maturity at length} components of the 
model can be defined as one of \code{c("full", "plat.full", "ramp", "plat.ramp", "logit", "plat.logit")} equations. The 
proper way to specify which function to use is by the construct of a \code{list} where the first element is the 
name of one of the six types of function. See example, read \code{\link{selectivity}}, or read \code{vignette("selectivity")} 
for more details.  

Alternatively, an object of class \code{"glm"} can directly be used for the \bold{fishery selectivity} and \bold{maturity at length} components. 
The Generalized Linear Model should have  the option \code{family} set to either \code{binomial} or \code{quasibinomial} keeping link function to the 
default (\eqn{i.e.} \code{"logit"}).
Estimated coefficients are use as follow: 
\tabular{c}{
\eqn{y=\frac{1}{1+e^{-(\alpha+\beta x)}}}\cr\cr 
}

\bold{REFERENCE POINTS:}\cr
Reference points used for result output are defined as follow:
\itemize{
    \item \bold{F.zero:}  F level when there is no fishing (F=0).
    \item \bold{F.01:}    F level where the slope of yield curve is 10\% of the slope at \code{F.zero}. 
    \item \bold{F.xx:}    F level where the MSP is at the level defined by \code{f.MSP} option. Default is 40\% (0.4).
    \item \bold{F.max:}   F level where yield is maximum.
}

Use \code{vignette("YPR")} for a better presentation of the equations.      

}
\value{  
  \code{ypr} returns an object of \code{class(S4)} \code{"ypr"}. The functions \code{summary}, \code{plot.sel.ypr}, and \code{plot} are used to respectively
  obtain a summary, plots for selectivity curves used, and a standard YPR plot of the results.  
  
  An object of class \code{"ypr"} has the the following slots:


  \item{parms}{the list of parameters used in the model.}
  \item{base}{a \code{data.frame} containing the starting values:
        \itemize{
            \item relative age classes
            \item length at age
            \item weight at age
        }
  }
  \item{refs}{a \code{data.frame} containing values predicted by the model for the four reference points. See details. }
  \item{YPR}{a \code{data.frame} containing the results for all partial Fs.}
  

  Note that to have access to each slot of an \code{"ypr"} object, one must use \code{"@"} instead of \code{"$"}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benoit Bruneau
François Grégoire
Diane Archambault
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.logit}}, \code{\link{plot.ypr}} and \code{\link{plot.parms.ypr}}  
}
\examples{

surv=function(x){
    p=x
    p[x<910]=0.8
    p[x>=910]=0.2
    p
}

ypr(vonB=c(1564.5508, 0.1206),  
        LW=c(0.000000009434 , 3.042974620048),
        l.start=241.86,
        last.age=14,
        age.step=1,
        prop.surv=surv,
        fish.lim=1000,
        Fsel.type=list(type="ramp",infl1=650,infl2=900, lv=c(0,0.2)),
        F.max=2, 
        F.incr.YPR=0.0001,
        Msel.type=list(type="ramp", infl1=400, infl2=600, lv=0.5, pos=F), 
        M=0.2, 
        Mat=list(type="ramp",infl1=700, infl2=900),
        f.MSP=0.4) 

}


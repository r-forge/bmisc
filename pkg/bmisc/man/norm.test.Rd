\name{norm.test}

\alias{norm.test}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normality tests
}
\description{
Lilliefors (Kolmogorov-Smirnov), Shapiro-Francia, Shapiro-Wilk, 
D'Agostino Skewness, Anscombe-Glynn Kurtosis and D'Agostino-Pearson normality tests.
}
\usage{
\method{norm.test}{default} (x, title=NULL, sk=c("G1","b1","mc"), type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{one of:
        \itemize{
            \item a numeric vector
            \item an object of class \code{c('lm','aov','glm','gam')} resulting from a
                calls to \code{c(\link{lm},\link{aov},\link{glm},\link{gam})}
        }
  }
  \item{title }{the title at the top of the results. Default is "Normality Tests".}
  \item{sk }{type of skewness used in D'Agostino skewness test. Can be \code{"G1"},\code{"b1"} or \code{"mc"}. Read details.}
  \item{type }{type of residuals which should be used. See details.}
}
\details{
This function can be used on objects belonging to \code{c('lm', 'aov', 'glm', 'gam')} classes. 
For example, \code{class(}aov.model\code{)} gives \code{"aov" "lm"} and \code{class(}glm.model\code{)} 
gives \code{"glm" "lm"}. The \code{type} of residuals can be defined. It generally includes 
\code{c("working", "response", "deviance", "pearson", "partial")}.

D'Agostino-Pearson's test is more appropriate for analysing a vector with
duplicate values in it. The more duplicate values in a vector, the more
Shapiro-Wilk will be far from correctly testing the \eqn{H0} hypothesis.

Given samples from a population, the equation for the sample skewness \eqn{g1}
is a biased estimator of the population skewness. The use of \eqn{G1} or \eqn{b1} is advisable.
For large samples, the various skewness estimates yield similar results. For small
normal distributed samples, \eqn{b1} is less biased than \eqn{G1}.
However, for small non-normal distributed samples, \eqn{G1} is less biased than \eqn{b1}.
These two skewness estimate can be sensitive to outliers in the data (contaminated data).
Therefore, the medcouple \code{\link{mc}} is also an option in \code{type}. It has a good
performance on uncontaminated data and is robust on contaminated data.
For more information on medcouple, please read references in \code{\link{mc}}\{\pkg{\link{robustbase}}\}.

\itemize{
    \item Typical definition used in many older textbooks:

  \tabular{c}{
  \eqn{g1=m3 / m2^(3/2)}
  }

where \eqn{m3} is the sample third central moment, and \eqn{m2} is the sample variance.\cr
}

\itemize{
    \item Definition used in SAS and SPSS:

   \tabular{c}{
   \eqn{G1=g1 * [k3/(k2^(3/2))] = g1 * [sqrt{n(n-1)} / (n-2)]}
   }

where \eqn{k3} is the unique symmetric unbiased estimator of the third
cumulant and \eqn{k2} is the symmetric unbiased estimator of the second cumulant.\cr
}

\itemize{
    \item Definition used in MINITAB and BMDP:
    \tabular{c}{
    \eqn{b1=m3 / s^3 = g1 ((n-1)/n)^(3/2)}
    }
  }


More will be added to this section especially for Anscombe-Glynn Kurtosis test.
}
\value{An S4 object of class 'norm' containig the following components:

  \item{statistics}{the statistics for each analysis}
  \item{p.value}{estimated p-values based on the statistics}
  \item{data}{original data (\code{data.frame})}
  \item{data.name }{names of the object called}
  \item{title}{title for the result}
}
\references{
  D. N. Joanes and C. A. Gill (1998),
  Comparing measures of sample skewness and kurtosis.
  \emph{The Statistician}, \bold{47}, 183--189.

  G. Brys, M. Hubert and A. Struyf (2003),
  A Comparison of Some NewMeasures of Skewness.
  in \emph{Developments in Robust Statistics} \bold{ICORS 2001},
  eds. R. Dutter, P. Filzmoser, U. Gather, and P.J. Rousseeuw, Heidelberg:
  Springer-Verlag, 98--113

  G. Brys, M. Hubert and A. Struyf (2004),
  A Robust Measure of Skewness;
  \emph{JCGS} \bold{13} (4), 996--1017.

}
\author{
Benoit Bruneau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{residuals}}, \code{\link{residuals.lm}}, \code{\link{residuals.glm}}, and \code{\link{residuals.gam}}
}
\examples{
x <- rnorm(300, 50, 10)
y  <- 5*(x +10*(rnorm(300,1,2)))

norm.test(x)            ## mc skewness
norm.test(x, type="G1") ## G1 skewness
norm.test(x, type="b1") ## b1 skewness

mod <- lm(y~x)
norm.test(mod)

}



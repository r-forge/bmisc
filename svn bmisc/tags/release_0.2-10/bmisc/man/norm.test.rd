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
\method{plot}{default} (norm.test(x, title=NULL, type=c("G1","b1","mc")))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{numeric vector or an object of class \code{"fREG"} such as lm, glm, aov , etc.}
  \item{title }{the title at the top of the results. Default is "Normality Tests".}
  \item{type }{type of skewness used in D'Agostino skewness test. Can be \code{"G1"},\code{"b1"} or \code{"mc"}. Read details.}
}
\details{
D'Agostino-Pearson's test is more appropriate for analysing a vector with
duplicate values in it. The more duplicate values in a vector, the more
Shapiro-Wilk will be far from correctly testing the \eqn{H0} hypothesis.

Given samples from a population, the equation for the sample skewness \eqn{g1}
is a biased estimator of the population skewness. The use of \eqn{G{_1}} or \eqn{b1} is advisable.
For large samples, the various skewness estimates yield similar results. For small
normal distributed samples, \eqn{b{_1}} is less biased than \eqn{G1}.
However, for small non-normal distributed samples, \eqn{G1} is less biased than \eqn{b1}.
These two skewness estimate can be sensitive to outliers in the data (contaminated data).
Therefore, the medcouple \code{\link{mc}} is also an option in \code{type}. It has a good
performance on uncontaminated data and is robust on contaminated data.
For more information on medcouple, please read references and/or type \code{\link{mc}} (\code{robustbase::mc}).

Here, d'Agostino skewness test is based on \code{mc} with default settings:

  \describe{
    \item{\eqn{g1=}}{
      \eqn{m3 / m2^(3/2)}.

      where \eqn{m3} is the sample third central moment, and \eqn{m2} is the sample variance.

      This is the typical definition used in many older textbooks.}

    \item{\eqn{G1=}}{
      \eqn{g1 * [k3/(k2^(3/2))] = g1 * [sqrt{n(n-1)} / (n-2)]}.

      where \eqn{k3} is the unique symmetric unbiased estimator of the third
      cumulant and \eqn{k2} is the symmetric unbiased estimator of the second cumulant.

      Used in SAS and SPSS.}

      \item{\eqn{b1=}}{
      \eqn{m3 / s^3 = g1 ((n-1)/n)^(3/2)}.

      Used in MINITAB and BMDP.}

  }


More will be added to this section especially for Anscombe-Glynn Kurtosis test.
}
\value{A list is returned with the following two components

  \item{D }{Lilliefor results}
  \item{W'}{Shapiro-Francia results}
  \item{W }{Shapiro-Wilk results}
  \item{Zb1 }{D'Agostino Skewness results}
  \item{Zb2 }{Anscombe-Glynn Kurtosis results}
  \item{Chi^2 }{D'Agostino Pearson results}

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
x <- rnorm(300, 50, 10)
histplot(x)
norm.test(x)            ## mc skewness
norm.test(x, type="G1") ## G1 skewness
norm.test(x, type="b1") ## b1 skewness

}



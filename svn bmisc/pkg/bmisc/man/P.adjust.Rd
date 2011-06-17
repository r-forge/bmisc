\name{P.adjust}

\alias{P.adjust}
\alias{P.adjust.methods}
\title{Adjust P-values for Multiple Comparisons}
\description{Given a set of p-values, returns p-values adjusted using
  one of several methods.  This is a modified version of \code{\link{p.ajust}} from \code{\link{stats}}.
  It now includes \code{"sidak"} correction.}
\usage{
P.adjust(p, method = P.adjust.methods, n = length(p))

P.adjust.methods
 c("holm", "hochberg", "hommel", "sidak", "bonferroni", "BH",
   "BY", "fdr", "none")
}
\arguments{
  \item{p}{vector of p-values (possibly with \code{\link{NA}}s).}
  \item{method}{correction method}
  \item{n}{number of pvalues considered for correction;
    only set this (to non-default) when you know what you are doing! See details}
}
\value{A vector of corrected p-values (same length as \code{p}).}
\details{
  The adjustment methods include the Bonferroni correction
  (\code{"bonferroni"}) in which the p-values are multiplied by the
  number of comparisons.  Less conservative corrections are also
  included by Holm (1979) (\code{"holm"}), Hochberg (1988)
  (\code{"hochberg"}), Hommel (1988) (\code{"hommel"}), Benjamini &
  Hochberg (1995) (\code{"BH"}), and Benjamini & Yekutieli (2001)
  (\code{"BY"}), respectively.
  A pass-through option (\code{"none"}) is also included.
  The \code{P.adjust.methods} vector containes the set of correction methods
  for the benefit of methods that need to have the method as an option
  and pass it on to \code{P.adjust}.

  The first five methods are designed to give strong control of the
  family wise error rate.  There seems no reason to use the unmodified
  Bonferroni correction because it is dominated by Holm's method, which
  is also valid under arbitrary assumptions.

  Hochberg's and Hommel's methods are valid when the hypothesis tests
  are independent or when they are non-negatively associated (Sarkar,
  1998; Sarkar and Chang, 1997).  Hommel's method is more powerful than
  Hochberg's, but the difference is usually small and the Hochberg
  p-values are faster to compute.

  The \code{"BH"} and \code{"BY"} method of Benjamini, Hochberg, and
  Yekutieli control the false discovery rate, the expected proportion of
  false discoveries amongst the rejected hypotheses.  The false
  discovery rate is a less stringent condition than the family wise
  error rate, so these methods are more powerful than the others.

  When making multiple comparisons, \code{n} can be used to identify the number
  of comparisons that are actually used.
  Correction is then done on the full output matrix when only some of the comparisons
  are meaningfull or chosen for hypothesis testing. This can be done with the \code{"bonferroni"}
  and \code{"sidak"} correction.
  If other methods are used, exclude the unwanted \code{p.values} before applying correction.
  Unless you know what you are doing, \bold{DO NOT} modify \code{n} if all comparisons are used. Most of the time \code{n}
  should be equal to \code{length(p)}.

  Note that you can set \code{n} larger than \code{length(p)} which
  means the unobserved p-values are assumed to be greater than all the
  observed p for \code{"bonferroni"} and \code{"holm"} methods and equal to 1 for
  the other methods.
}

\references{
  Benjamini, Y., and Hochberg, Y. (1995).
  Controlling the false discovery rate: a practical and powerful
  approach to multiple testing.
  \emph{Journal of the Royal Statistical Society Series} B, \bold{57},
  289--300.

  Benjamini, Y., and Yekutieli, D. (2001).
  The control of the false discovery rate in multiple testing under
  dependency.
  \emph{Annals of Statistics} \bold{29}, 1165--1188.

  Holm, S. (1979).
  A simple sequentially rejective multiple test procedure.
  \emph{Scandinavian Journal of Statistics}, \bold{6}, 65--70.

  Hommel, G. (1988).
  A stagewise rejective multiple test procedure based on a modified
  Bonferroni test.
  \emph{Biometrika}, \bold{75}, 383--386.

  Hochberg, Y. (1988).
  A sharper Bonferroni procedure for multiple tests of significance.
  \emph{Biometrika}, \bold{75}, 800--803.

  Shaffer, J. P. (1995).
  Multiple hypothesis testing.
  \emph{Annual Review of Psychology}, \bold{46}, 561--576.
  (An excellent review of the area.)

  Sarkar, S. (1998).
  Some probability inequalities for ordered MTP2 random variables: a
  proof of Simes conjecture.
  \emph{Annals of Statistics}, \bold{26}, 494--504.

  Sarkar, S., and Chang, C. K. (1997).
  Simes' method for multiple hypothesis testing with positively
  dependent test statistics.
  \emph{Journal of the American Statistical Association}, \bold{92},
  1601--1608.

  Wright, S. P. (1992).
  Adjusted P-values for simultaneous inference.
  \emph{Biometrics}, \bold{48}, 1005--1013.
  (Explains the adjusted P-value approach.)
}

\seealso{
\code{\link{pairwise.t.test}}, \code{\link{mc.long}}, \code{\link{DTK.test}}, \code{\link{TukeyHSD}} and \code{\link{glht}}
}

\examples{
require(graphics)

set.seed(123)
x <- rnorm(50, mean=c(rep(0,25),rep(3,25)))
p <- 2*pnorm( sort(-abs(x)))

round(p, 3)
round(P.adjust(p), 3)
round(P.adjust(p,"BH"), 3)

## or all of them at once (dropping the "fdr" alias):
P.adjust.M <- P.adjust.methods[P.adjust.methods != "fdr"]
p.adj <- sapply(P.adjust.M, function(meth) P.adjust(p, meth))
round(p.adj, 3)
## or a bit nicer:
noquote(apply(p.adj, 2, format.pval, digits = 3))

## and a graphic:
matplot(p, p.adj, ylab="P.adjust(p, meth)", type = "l", asp=1, lty=1:6,
        main = "P-value adjustments")
legend(.7,.6, P.adjust.M, col=1:6, lty=1:6)

## Can work with NA's:
pN <- p; iN <- c(46,47); pN[iN] <- NA
pN.a <- sapply(P.adjust.M, function(meth) P.adjust(pN, meth))
## The smallest 20 P-values all affected by the NA's :
round((pN.a / p.adj)[1:20, ] , 4)
}


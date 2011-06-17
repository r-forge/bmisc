\name{corr.perm}

\alias{corr.perm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pearson Correlation by Permutation
}
\description{
Tests the Pearson correlation estimate (r) by use of permutation
}
\usage{
corr.perm(x,y,nperm=999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{Two vectors of same length used for correlation analysis}
  \item{nperm}{Number of permutations (default = 999)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Correlation }{Pearson r}
\item{t.stat }{Calculated test statistic (t)}
\item{No.perm }{number of permutations}
\item{P.perm }{pvalue estimated by permutations}
\item{P.para }{parametric pvalue estimated}
\item{inf }{inferior limit of the confidence interval}
\item{sup }{superior limit of the confidence interval}
\item{df }{degree of freedom}
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
x <- rnorm(50,0,1)
y <- runif(50,0,1)*x
toto = corr.perm(x, y)
}


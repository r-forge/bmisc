\name{ttest.perm}

\alias{ttest.perm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Student's t-tests by Permutation
}
\encoding{utf8}
\description{
Performs two sample t-tests  or paired t-test by use of permutation
}
\usage{
ttest.perm(vec1, vec2, nperm=999, alternative = "two.sided",
           var.equal = T, silent=FALSE, type="i", exact=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec1, vec2 }{
two numeric vectors used for Student's t-test analysis
}
  \item{nperm }{
number of permutations (default = 999)
}
  \item{alternative }{
one of the following: "two.sided", "less" or "greater".
}
  \item{var.equal }{
a logical variable indicating whether to treat the two variances as being equal (\code{TRUE}) or not (\code{FALSE}).
}
  \item{silent }{
a logical variable indicating whether calculation results are printed (\code{FALSE}) to the R console or not (\code{TRUE}).
}
  \item{type }{
one of the following: "i" for independant samples or "p" for paired samples.
}
  \item{exact }{
a logical variable indicating whether to perform the exact test (\code{TRUE}) or not (\code{FALSE}).
}
}
\details{
The permutational t-test does not require normality of the distributions of each variable.
It is also quite robust to heteroscedasticity.

Use \code{exact=TRUE} to perform two sample t-test on all the possible combination.
This option can only be used when the sum of the sample sizes \eqn{(n{_1}+n{_2})} is smaller than 20.
It is recommended to use this option when sample sizes are small.
It is not implemented yet in the paired t-test.

\code{nperm} can not be higher than the maximum number of combination possible (\eqn{n_{comb}}).

\describe{
    \item{\eqn{n_{comb} =}}{\eqn{N! / (n{_1}!n{_2}!)}


      where \code{n_comb} is the number of possible combinations, \eqn{N!}
      is \code{\link{factorial}\eqn{(n{_1}+n{_2})}}, \eqn{n{_1}!} is
      \code{\link{factorial}(\link{n}(vec1))} and \eqn{n{_2}!} is
       \code{\link{factorial}(\link{n}(vec2))}.}}

There is more to come in this section.

}
\value{
\item{t.ref }{reference value of the t-statistic}
\item{p.param }{parametric p-value}
\item{p.perm }{permutational p-value}
\item{nperm }{number of permutations}
\item{perm.t }{list of the t statistics (only for independant sample ttest), starting with the reference value, followed by all values obtained under permutations.}
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
toto = ttest.perm(x, y)  ##independant samples ttest
}

\keyword{ ttest }




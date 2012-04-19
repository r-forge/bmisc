\name{plot.lev}
\alias{plot.lev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Diagnostics for an \code{lev} Object
}
\description{
Four boxplots (selectable by \code{which}) are available.
}
\usage{
plot(obj, which=1L:4L, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object of class \code{'lev'} resulting from a calls to \code{\link{lev}}.
}
  \item{which}{numerical vector indicating the subset of the plots required:
     \itemize{
            \item \code{1} : residuals of mean (Levene)
            \item \code{2} : residuals of trimmed mean (Robust Levene)
            \item \code{3} : residuals of median (Brown-Forsythe)
            \item \code{3} : r scores (O'Brien)
      }
}
\item{\dots}{arguments to be passed to \code{\link{boxplot}}.
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
z=data.frame( yy=c(rep("c",50),rep("d",50)),
        x=c(rnorm(50),rnorm(50,10)),
        s=rep(c(rep("a",25),rep("b",25)),2),
        qq=rep(c(rep("w",10),rep("t",10)),5))

mod=lm(x~yy*qq*s, data=z)
res=lev(mod)

plot(res)
}


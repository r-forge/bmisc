\name{plot.mat}
\alias{plot.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard plot for maturity ogive
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.mat(object, se.pred=TRUE, leg=TRUE, ref=TRUE, range.x=c("data","full"), 
         warn.val=TRUE, ylab="Probabilité",xlab="Longueur à la fourche (mm)")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{'glm'} resulting from a call to \code{\link{glm}}.
}
  \item{se.pred}{logical; if TRUE, SE is plotted.
}
  \item{leg}{logical; if TRUE, a legend containing logistic equation and estimated values for the variables is plotted.
}
  \item{ref}{logical; if TRUE, reference lines for \code{L90, L50 and L10} are plotted.
}
  \item{range.x}{the range used to define \code{xlim} in the plot. Read 'details'.
}
}
\details{
More to come.
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

}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

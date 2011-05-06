\name{clean}

\alias{clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean a Data Frame
}
\description{
Cleans a \code{data.frame} from a starting point with a defined threshold
}
\usage{
clean(data= x,  col.start =1,  min.val=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{then name of the \code{data.frame}}
  \item{col.start}{indicate the columns from which to start reading }
  \item{min.val}{\code{numeric}. Read details}
}
\details{
\code{min.val} is the minimum value accepted in a column. Colomns with this value or higher will be kept in the \code{data.frame}.

More will be added to this function.
}
\value{
returns the data.frame with the clean columns
}
\references{

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
x=rnorm(50 , 20,  12)
y=runif(50 )
z=rpois(50,  3)
v=x*y/z
t=z*v
pp=data.frame(aa=x,  bb=y, cc=v,  dd=z,  ee=t)
summary(pp)

pp1 = clean(pp, min.val=0.06)
}


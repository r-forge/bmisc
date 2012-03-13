\name{rivard}
\alias{rivard}

\title{
Rivard Weight Calculations
}
\description{
This function applies Rivard equations to mid-year weight at age data to adjust values to Jan-1 basis. 
}
\usage{
rivard(data, pred=FALSE, K=2, plus.gr=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a \code{\link{data.frame}} where each column has the weight at age for of a year. There should be as much columns as there are years.}
  \item{pred}{logical. Indicates if predictions should be made (\code{TRUE}) for year+1.}
  \item{K}{number of years to use for averaging when \code{pred=TRUE}.}
  \item{plus.gr}logical. Indicates if the the oldest age of each year is a plus groupe (\code{TRUE}) or not (\code{FALSE}).}  
}
\details{
In \code{\link{ypr}}, the \code{rivard} option can do modified Rivard Weight Calculations when \code{age.step} is less than \code{1}.
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
Benoit Bruneau and\cr
François Grégoire
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ypr}}
}
\examples{

x=1:10
y=data.frame( y1996=x,y1997=x+0.1,y1998=x+0.5)

rivard(data=y,pred=TRUE,K=2)

}


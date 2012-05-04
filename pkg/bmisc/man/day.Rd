\name{day}

\alias{day}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
day
}
\description{Day of year as decimal number (001-366).}
\usage{
day(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an objects of classes \"POSIXlt\" and \"POSIXct\".
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

dates=strptime(c(\"02/27/92\", \"02/27/92\", \"01/14/92\", \"02/28/92\", \"02/01/92\"),format=\"%m/%d/%y\")

day(dates)
}


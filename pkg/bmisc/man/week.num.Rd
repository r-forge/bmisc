\name{week.num}

\alias{week.num}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
week.num
}
\description{Week of the year as decimal number (00-53) using Sunday or Monday as 
the first day 1 of the week (and typically with the first Sunday of the year as day 1 of week 1).}
\usage{
week.num(x, day=c("sunday", "monday"))
}

\arguments{
  \item{x}{
A vector of dates.
}
  \item{day}{
Either "sunday" or "monday". Default is "sunday".
}
}
\details{
Argument \code{day} indicates if the week starts on \code{"sunday"} or \code{"monday"}.
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
dated <-as.Date(c("2006-05-18","2006-05-07","2006-04-23",
                  "2006-04-24","2006-05-07","2007-05-17",
                  "2007-05-06","2007-04-22","2007-04-29"))
                  
week.num(dated, "monday")
week.num(dated)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


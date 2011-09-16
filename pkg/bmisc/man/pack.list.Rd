\name{pack.list}
\alias{pack.list}

\title{
List of installed packages
}
\description{
Adds the list of the packages currently installed in R to Rprofile.
}
\usage{
pack.list(rptofile=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rprofile}{
logical. Should the list of installed packages be added to the Rprofile (\code{TRUE}) or saved in a text file (\code{FALSE})
}
}
\details{
\bold{rprofile = \code{TRUE}}\cr
A \code{SAVE} dialog box opens.


\bold{rprofile = \code{TRUE}}\cr
For Mac and Linux users, \code{.Rprofile} is either modified or created. On Windows systems, users are prompted for 
\code{Rprofile.site} location. This File is usually in \code{"C:/Program Files/R/R-x.xx.x/etc"}. Once it's selected, it is 
modified to include the list of packages currently installed in R.

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
pack.list()
pack.list(5)
}
\name{show.North}

\alias{show.North}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
North arrow for a map
}
\description{
Draws North arrow on a map
}
\usage{
show.North(pos, arrow.col="black", arrow.fill="black", arrow.lwd=1,
           N.cex=1, N.family="HersheyGothicEnglish")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{Position of the arrow. Default is \code{'topright'}. See details.
}
  \item{arrow.col}{Arrow color.
}
  \item{arrow.fill}{Color inside the head of the arrow. \code{NA} for no color.
}
  \item{arrow.lwd}{Line width of the arrow.
}
  \item{N.cex}{Character size for 'N'.
}
  \item{N.family}{Font family of 'N'.
}

}
\details{
The position of the north arrow is defined by \code{pos} and can either be numeric or character.

If \code{pos} is numeric, it is a vector of the form \code{c(x,y)} where \code{x} and \code{y} are fractions of the plotting region.
If \code{x} and \code{y} are not in the range of \code{c(0,1)}, then the north arrow is drawn ouside the bounds of the plotting region 
and a warning message is given.

If \code{pos} is character, it is one of \code{c('topright','topleft','bottomright','bottomleft')}.

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

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
plot(1)
show.North()
show.North(c(0.8,0.9))
show.North(c(1.01,0.9))  ### gives a warning
}


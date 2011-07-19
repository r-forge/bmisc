\name{histplot}

\alias{histplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
histplot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
histplot(dat, breaks="Sturges", barc="steelblue", borc="white",
         fit.norm=TRUE, lcol="brown", stat=NULL, 
         stat.lab=c("Mean","Median"), box=TRUE, rug=TRUE, 
         main,...)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{one of:
    \itemize{
      \item a numeric vector,
      \item an object of class \code{"norm"}resulting from a
    		call to \code{\link{norm.test}}}
  \item{breaks}{one of:
    \itemize{
      \item a vector giving the breakpoints between histogram cells,
      \item a single number giving the number of cells for the histogram,
      \item a character string naming an algorithm to compute the
      number of cells (see \sQuote{Details}),
      \item a function to compute the number of cells.
    }
    In the last three cases the number is a suggestion only.
  }
  \item{barc}{a color to be used to fill the bars.}
  \item{borc}{a color to be used for the borders the bars.}
  \item{fit.norm}{a logical variable indicating whether to fit a normal density
             curve \code{(TRUE)} or not \code{(FALSE)}.}
  \item{lcol}{color of the normal density curve}
  \item{stat}{the statistic to add on the graph. One of (\code{c("all","mean","median")}). Default is \code{NULL}.}
  \item{stat.lab}{a character vector with the labels for the estimated mean and/or median. Default is \code{c("Mean","Median")}.}
  \item{rug}{a logical variable indicating whether to superpose a \code{\link{rug}}
             \code{(TRUE)} or not \code{(FALSE)}.}
  \item{main}{the main title of the graph}
  \item{\dots}{additional arguments to be passed to plot (see \code{\link{par}})}

}
\details{
  The default for \code{breaks} is \code{"Sturges"}: see
  \code{\link{nclass.Sturges}}.  Other names for which algorithms
  are supplied are \code{"Scott"} and \code{"FD"} /
  \code{"Freedman-Diaconis"} (with corresponding functions
  \code{\link{nclass.scott}} and \code{\link{nclass.FD}}).
  Alternatively, a function can be supplied which
  will compute the intended number of breaks as a function of \code{x}.
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
\code{\link{hist}}
}
\examples{
x=rnorm(50)
histplot(x)
}


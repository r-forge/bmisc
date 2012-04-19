\name{Errbar}

\alias{Errbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Error Bars
}
\description{
Adds error bars on a plot
}
\usage{
Errbar(x, y, xinf=NULL, xsup=NULL, yinf=NULL, ysup=NULL, xint=NULL,
       xint=NULL, cap=0.05,\dots)
}

\arguments{
  \item{x }{numeric vector
  }
  \item{y}{numeric vector
  }
  \item{xinf, xsup}{numeric vectors containing the upper (xsup) and/or lower (xinf)
        limits of the confidence interval for x-axis values.
  }
  \item{yinf, ysup}{numeric vectors containing the upper (ysup) and/or lower (yinf)
        limit of the confidence interval for y-axis values.
  }
  \item{xint}{numeric vectors containing the difference between upper and lower limits (intervals) for x-axis values.
  }
  \item{yint}{numeric vectors containing the difference between upper and lower limits (intervals) for y-axis values.
  }
  \item{cap}{length of the caps on the error bars.
  }
  \item{\dots}{additional graphical arguments arguments for \code{\link{arrows}} .
}
}
\details{
If \code{xint} and/or \code{yint} are defined, individually defined limits (ie. \code{xinf, xsup, yinf, ysup}) are not used.
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
\code{\link{arrows}}, \code{\link{par}}
}
\examples{
x <- 1:10
y <- x + rnorm(10)

yci <- runif(10)
xci <- runif(10)

plot(x,y, ylim=c(min(y-yci),max(y+yci)))
Errbar( x, y, yint=yci)

plot(x,y, xlim=c(min(x-xci),max(x+xci)))
Errbar( x, y, xint=xci )

plot(x,y, ylim=c(min(y-yci),max(y+yci)), xlim=c(min(x-xci),max(x+xci)))
Errbar( x, y, yint=yci, xint=xci )

# Gives an Error message
#plot(x,y, ylim=c(min(y-yci),max(y+yci)))   ## adds the yint and gives
#Errbar( x, y, ysup=1, yint=yci)            ## an error message for the ysup

}


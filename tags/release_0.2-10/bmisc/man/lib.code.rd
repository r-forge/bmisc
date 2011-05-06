\name{lib.code}

\alias{lib.code}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retreives the code for \code{lib()}.
}
\description{
Will print in the R windows the code for \code{lib()} \bold{(READ DETAILS)}. 
}
\usage{
lib.code()
lib(pack, install=TRUE, load=TRUE, quietly=TRUE, 
    warn.conflicts=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pack}{Character vector specifying which package(s) to load/install.
}
}
\details{

\bold{USE \code{lib.code()} TO GET THE CODE FOR THE FUNCTION \code{lib()}.}


\code{lib.code()} prints in R the code for \code{lib()}. Copy and paste the code for \code{lib()}  
in the file \code{"C:/Program Files/R/R-2.12.1/etc/Rprofile.site"} (Windows) or \code{"~/.Rprofile"} (Mac).


\code{lib()} will load packages named in a charcater vector. If install is \code{TRUE}, 
packages not yet installed will be installed.
 
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

lib.code()

}


\name{att.strp}

\alias{att.strp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attibute stripper
}
\description{
Strips an object of its attributes
}
\usage{
att.strp(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{the name of an object (\code{vector, matrix, data.frame, array or list})}
}
\details{
This function strips an object of its attributes. In the case of a \code{vector}, all attributes are removed. 
For a \code{matrix} or an \code{array}, only \code{c('dim','dimnames')} are kept. When \code{att.strp} is used on a \code{data.frame}, 
all attributes of the variables are striped and only \code{c('names','row.names','na.action', 'class')} are kept for the
\code{data.frame} object.  


}
\value{
returns an object of the same \code{class} as \code{obj}.
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

######################################
#   Creating different objects       #
#    with added attributes (label)   #
######################################

### numerical vector ###
x <- 1:10
attr(x,"label") <- "test1"
attributes(x)

### data frame ###
z=data.frame(x,x)
attr(z,"labels") <- "test2"
attributes(z)
attributes(z[,1])
attributes(z[,2])

### array ###
y=array(x,c(2,2,2))
attr(y,"labels") <- "test3"
attributes(y)
attributes(y[,,1])
attributes(y[,,2])

### list containing the vector, ###
### data frame and array        ###
u=list(x,z,y)
attr(u,"labels") <- "test4"
attributes(u)
attributes(u[[1]])
attributes(u[[2]])
attributes(u[[3]])

######################################
#        attribute stripping         #
######################################
x2=att.strp(x)
z2=att.strp(z)
y2=att.strp(y)
u2=att.strp(u)

######################################
#   verification of the attributes   #
#    for all stripped objects        #
######################################

### numerical vector ###
attributes(x2)

### data frame ###
attributes(z2)
attributes(z2[,1])
attributes(z2[,2])

### array ###
attributes(y2)
attributes(y2[,,1])
attributes(y2[,,2])

### list containing the vector, ###
### data frame and array        ###
attributes(u2)
attributes(u2[[1]])       # vector in the list

attributes(u2[[2]])       # data frame in the list
attributes(u2[[2]][,1])   # data frame in the list
attributes(u2[[2]][,2])   # data frame in the list

attributes(u2[[3]]        # array in the list
attributes(u2[[3]][,,1])  # array in the list
attributes(u2[[3]][,,2])  # array in the list



}


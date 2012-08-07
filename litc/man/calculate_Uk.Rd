\name{calculate_Uk}
\alias{calculate_Uk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that calculates blabla
}
\description{
Description of Calculate_UK
}
\usage{
calculate_Uk(x, centers, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{centers}{
%%     ~~Describe \code{centers} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,centers,m){
  # For each pattern
  U <- matrix(nrow=nrow(x),ncol=nrow(centers))
  for(i in 1:nrow(x)){
    U[i,] <- calculate_membership(x[i,],centers,m)
  }
  return(U)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{calculate_membership}
\alias{calculate_membership}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Membership of the Patterns for a cluster.
}
\description{
Calculate Test!
}
\usage{
calculate_membership(pattern, centers, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
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
function(pattern,centers,m){
  # Calculate the euclidian distance for all the centers
  membership <- matrix(nrow=1,ncol=nrow(centers));
  distances <- sqrt(rowSums((pattern - centers)^2))
  fuzzy_factor <- 2/(m-1)
  for(i in 1:ncol(membership)){
    factors <- distances[i] / distances
    membership[1,i] <- 1/sum(factors^(fuzzy_factor))
  }
  return(membership)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

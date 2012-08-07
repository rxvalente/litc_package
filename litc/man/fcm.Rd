\name{fcm}
\alias{fcm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FCM Funcion
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fcm(x, c, m = 2, iter.max = 100, tol = sqrt(.Machine$double.eps), centers)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{c}{
%%     ~~Describe \code{c} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{iter.max}{
%%     ~~Describe \code{iter.max} here~~
}
  \item{tol}{
%%     ~~Describe \code{tol} here~~
}
  \item{centers}{
%%     ~~Describe \code{centers} here~~
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
function (x, c, 
                 m = 2,
                 iter.max = 100,
                 tol = sqrt(.Machine$double.eps),
                 centers)
{
  
  x <- as.matrix(x)
  xrows <- nrow(x)
  xcols <- ncol(x)
  
  if (missing(c)) 
    stop("Number of clusters 'c' is not defined!")
  
  if(missing(centers)){
    cmin <- range(x)[1]
    xmax <- range(x)[2]
    cvector <- runif(c*xcols)
    centers <- matrix(data=cvector,nrow=c,ncol=xcols)
  }else{
    centers <- as.matrix(centers)
    crows <- nrow(centers)
    ccols <- ncol(centers)
    if(crows != c || ccols != xcols){
      stop("The center matrix must have 'c' rows and the 'n' columns!")
    }
  }
  
  initcenters <- centers
  U <- matrix(0,nrow=nrow(x),ncol=nrow(centers))
  # Algorithm starts here
  for(i in 1:iter.max){
    Uk <- calculate_Uk(x,centers,m)
    centers <- calculate_centers(x,Uk,m)
    max_diff = max(abs(Uk - U))
    #print(max_diff)
    if(max_diff < tol){
      break;
    }
    U <- Uk  
  }
  # Algorithm ends here
  
  result = list(U=U,centers=centers,iter=i)
  return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

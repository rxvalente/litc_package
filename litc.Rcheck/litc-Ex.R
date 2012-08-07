pkgname <- "litc"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('litc')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("calculate_Uk")
### * calculate_Uk

flush(stderr()); flush(stdout())

### Name: calculate_Uk
### Title: Function that calculates blabla
### Aliases: calculate_Uk
### Keywords: ~kwd1 ~kwd2

### ** Examples

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



cleanEx()
nameEx("calculate_centers")
### * calculate_centers

flush(stderr()); flush(stdout())

### Name: calculate_centers
### Title: Calcula os centros
### Aliases: calculate_centers
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x,U,m){
  
  centers <- matrix(nrow=ncol(U),ncol=ncol(x))
  U_fuzzy <- U^m
  sum_clusters <- colSums(U_fuzzy)
  for(i in 1:nrow(centers)){
    fuzzy_factors <- U_fuzzy[,i]
    centers[i,] <- colSums(fuzzy_factors*x) / sum_clusters[i]
  }
  
  return(centers)
  }



cleanEx()
nameEx("calculate_membership")
### * calculate_membership

flush(stderr()); flush(stdout())

### Name: calculate_membership
### Title: Calculate the Membership of the Patterns for a cluster.
### Aliases: calculate_membership
### Keywords: ~kwd1 ~kwd2

### ** Examples

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



cleanEx()
nameEx("fcm")
### * fcm

flush(stderr()); flush(stdout())

### Name: fcm
### Title: FCM Funcion
### Aliases: fcm
### Keywords: ~kwd1 ~kwd2

### ** Examples

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



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

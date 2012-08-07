# LITC Package
# Fuzzy C Means Algorithm
# Author: Rafael Xavier Valente
# Last Updated: 01/08

################################ PARAMETERS ################################
#
# x : Data Matrix N x n, where N is the number of patterns and n is the number of clusters
# c : Number of clusters
# iter.max : Maximum number of iterations
# tol : Limit for the difference of max(abs(Uk -U))
#
############################################################################
fcm <- function (x, c, 
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

# Calculates the matrix U for each iteration
calculate_Uk <- function(x,centers,m){
  # For each pattern
  U <- matrix(nrow=nrow(x),ncol=nrow(centers))
  for(i in 1:nrow(x)){
    U[i,] <- calculate_membership(x[i,],centers,m)
  }
  return(U)
}

# Calculates the membership for a pattern
calculate_membership <- function(pattern,centers,m){
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

# Calculates the new centers based on Uk
calculate_centers <- function(x,U,m){
  
  centers <- matrix(nrow=ncol(U),ncol=ncol(x))
  U_fuzzy <- U^m
  sum_clusters <- colSums(U_fuzzy)
  for(i in 1:nrow(centers)){
    fuzzy_factors <- U_fuzzy[,i]
    centers[i,] <- colSums(fuzzy_factors*x) / sum_clusters[i]
  }
  
  return(centers)
}
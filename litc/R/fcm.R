fcm <-
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


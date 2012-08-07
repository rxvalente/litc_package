calculate_centers <-
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


calculate_membership <-
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


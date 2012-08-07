calculate_Uk <-
function(x,centers,m){
  # For each pattern
  U <- matrix(nrow=nrow(x),ncol=nrow(centers))
  for(i in 1:nrow(x)){
    U[i,] <- calculate_membership(x[i,],centers,m)
  }
  return(U)
}


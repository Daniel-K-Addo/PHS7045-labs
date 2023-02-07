

# Definition of outcome given treatment effect within each arm
outcomeDefinition <- function(i,j,Y,n_allocate,trt_effect){
  # Definition of outcome given treatment effect within each arm
  if(j==1){
    Y[[i]] <- rbinom(n_allocate[i],1,trt_effect[i])
  }else{
    Y[[i]] <- c(Y[[i]],rbinom(n_allocate[i],1,trt_effect[i]))
  }
  return(Y[[i]])
}

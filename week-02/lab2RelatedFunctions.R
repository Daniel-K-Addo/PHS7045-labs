# allocationUpdate
## Update allocation probability after posterior draws
allocationUpdate <- function(post_y_all, n_samp){
  pt_max0 <- apply(post_y_all, 1, which.max) # Identify trt with max prob
  pt_max <- c(mean(pt_max0==1), mean(pt_max0==2),
              mean(pt_max0==3), mean(pt_max0==4)) # Compute relative freq
  V_0 <- min(sum(pt_max[-1] * (n_samp[-1]+1)/(n_samp[1]+1)),
             max(pt_max[-1])) # Allocation prob for control
  V_t <- c(V_0, pt_max[-1]) # Allocation probs for all treatments
  V_t_norm <- V_t/sum(V_t) # Normalize allocation probabilities
  return(V_t_norm)
}

# outcomeDefinition
## Definition of outcome given treatment effect within each arm
outcomeDefinition <- function(i,j,Y,n_allocate,trt_effect){
  # Definition of outcome given treatment effect within each arm
  if(j==1){
    Y[[i]] <- rbinom(n_allocate[i],1,trt_effect[i])
  }else{
    Y[[i]] <- c(Y[[i]],rbinom(n_allocate[i],1,trt_effect[i]))
  }
  return(Y[[i]])
}

# treatmentComparison
## Compare treatments with control and select best
treatmentComparison <-  function(trt_n,post_y,n_samp,design_option){
  comparison_means <- sapply(2:(trt_n), function(x) mean(post_y[[x]]>post_y[[1]]))
  max_trt <- which.max(comparison_means) # which trt is best
  max_trt_prob<- comparison_means[which.max(comparison_means)] #best trt prob
  output <- NULL
  # Print output
  # the prob that best trt is better than ctrl
  output[[1]] <- noquote(paste("The best treatment arm with probability of",
                               "being better than control is treatment",
                               max_trt, "with probability",
                               round(max_trt_prob,2)
  ))
  # Number of patients assigned to each trial
  output[[2]] <- cbind(Treatment = 0:(trt_n-1),
                       Size = n_samp)
  
  # Was the trial successful?
  delta_1 <- 0.9912
  delta_2 <- 0.9892
  if(design_option == 1){
    output[[3]] <- ifelse(max_trt_prob > delta_1,
                          noquote(paste("Trial was successful")),
                          noquote(paste("Trial was NOT successful"))
    )
  }else{
    output[[3]] <- ifelse(max_trt_prob > delta_2,
                          noquote(paste("Trial was successful")),
                          noquote(paste("Trial was NOT successful"))
    )
  }
  return(output)
}

# Compare treatments with control and select best
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

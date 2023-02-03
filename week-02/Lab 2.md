Lab 2
================

# Design 1

``` r
design_one <- function(N_samp = 228, # Total number of participants
                       trt_n = 4, # Number of Arms
                       trt_effect = rep(0.35,4), # treatment effect for each arm 
                       alpha = 0.35,  beta = 0.65, # prior
                       n_post = 1000, # number of posterior draws
                       seed_setting = 4832, # set seed for replication
                       allocation_size = 40 # Initial Sample to allocate
                       ){
  set.seed(seed_setting)
  n_samp <- N_samp/trt_n
  Y <- NULL
  post_y <- NULL
  for(i in 1:trt_n){
    # Definition of outcome given treatment effect within each arm
    Y[[i]] <- rbinom(n_samp,1,trt_effect[i])
    # Posterior draws
    post_y[[i]] <- rbeta(n_post, 
                         alpha + sum(Y[[i]]), 
                         beta + n_samp - sum(Y[[i]])
                         )
  }
  # Compare treatments with control and select best
  comparison_means <- sapply(2:(trt_n), function(x) mean(post_y[[x]]>post_y[[1]]))
  max_trt<-which.max(comparison_means)
  max_trt_prob <- comparison_means[max_trt]
  output <- NULL
  output[[1]] <- noquote(paste("The best treatment arm with probability of",
                      "being better than control is treatment",
                       max_trt, "with probability",
                       round(max_trt_prob,2)
                       ))
  output[[2]] <- cbind(Treatment = 0:(trt_n-1),
                       Size = rep(n_samp,trt_n))
  return(output)
}

design_one(N_samp = 228, # Total number of participants
           trt_n = 4, # Number of Arms
           trt_effect = rep(0.35,4), # treatment effect for each arm 
           alpha = 0.35,  beta = 0.65, # prior
           n_post = 1000, # number of posterior draws
           seed_setting = 4832, # set seed for replication
           allocation_size = 40 # Initial Sample to allocate
           )
```

    [[1]]
    [1] The best treatment arm with probability of being better than control is treatment 3 with probability 0.46

    [[2]]
         Treatment Size
    [1,]         0   57
    [2,]         1   57
    [3,]         2   57
    [4,]         3   57

# Design 2 (With the option of Design 1)

``` r
design_two <- function(N_samp = 228, # Total number of participants
                       trt_n = 4, # Number of Arms
                       trt_effect = rep(0.35,4), # treatment effect for each arm 
                       alpha = 0.35,  beta = 0.65, # prior
                       n_post = 1000, # number of posterior draws
                       seed_setting = 4832, # set seed for replication
                       allocation_size = 40, # Initial Sample to allocate
                       design_option = 2 # Design option 
                       ){
  set.seed(seed_setting) # Set seed
  # Condition to revert to design one:
  allocation_size <- ifelse(design_option==1, N_samp, allocation_size)
  # Compute # of interim allocation steps needed
  interim_steps <- ceiling(N_samp/allocation_size) 
  n_samp <- NULL
  Y <- NULL
  for(j in 1:interim_steps){
    post_y <- NULL
    post_y_all <- NULL
    # Define allocation probabilities
    if(j==1){ # First time, allocate equally across treatments
      allocation_prob <- rep(1,trt_n)/trt_n
      n_allocate <- round(allocation_prob*allocation_size)
      n_allocate[trt_n] <- allocation_size - sum(n_allocate[-trt_n])
      n_samp <- n_allocate
    }else if(j!=interim_steps){ # Allocate according to allocation probs across treatments
      allocation_prob <- V_t_norm
      n_allocate <- round(allocation_prob*allocation_size)
      n_allocate[trt_n] <- allocation_size - sum(n_allocate[-trt_n])
      n_samp <-n_allocate+n_samp
    }else{ # Allocate remaining sample size
      allocation_prob <- V_t_norm
      n_allocate <- round(allocation_prob*(N_samp %% allocation_size))
      n_allocate[trt_n] <- (N_samp %% allocation_size) - sum(n_allocate[-trt_n])
      n_samp <-n_allocate+n_samp
    } 
  for(i in 1:trt_n){
    # Definition of outcome given treatment effect within each arm
    if(j==1){
      Y[[i]] <- rbinom(n_allocate[i],1,trt_effect[i])
    }else{
      Y[[i]] <- c(Y[[i]],rbinom(n_allocate[i],1,trt_effect[i]))
    }
    # Posterior draws
    post_y[[i]] <- rbeta(n_post, 
                         alpha + sum(Y[[i]]), 
                         beta + n_samp[i] - sum(Y[[i]])
                         )
    post_y_all <- cbind(post_y_all,post_y[[i]])
  }
  pt_max0 <- apply(post_y_all, 1, which.max) 
  pt_max <- c(mean(pt_max0==1),
              mean(pt_max0==2),
              mean(pt_max0==3),
              mean(pt_max0==4)
              )
  
  # Update allocation probability
  V_0 <- min(sum(pt_max[-1] * (n_samp[-1]+1)/(n_samp[1]+1)),
             max(pt_max[-1]))
  V_t <- c(V_0, pt_max[-1])
  V_t_norm <- V_t/sum(V_t) # Normalize to get allocation probabilities
  }
  
  
  # Compare treatments with control and select best
  comparison_means <- sapply(2:(trt_n), function(x) mean(post_y[[x]]>post_y[[1]]))
  max_trt<-which.max(comparison_means)
  max_trt_prob <- comparison_means[max_trt]
  output <- NULL
  output[[1]] <- noquote(paste("The best treatment arm with probability of",
                      "being better than control is treatment",
                       max_trt, "with probability",
                       round(max_trt_prob,2)
                       ))
  output[[2]] <- cbind(Treatment = 0:(trt_n-1),
                       Size = n_samp)
  return(output)
}

design_two(N_samp = 228, # Total number of participants
           trt_n = 4, # Number of Arms
           trt_effect = rep(0.35,4), # treatment effect for each arm 
           alpha = 0.35,  beta = 0.65, # prior
           n_post = 1000, # number of posterior draws
           seed_setting = 1030, # set seed for replication
           allocation_size = 40, # Initial Sample to allocate
           design_option = 2 # Design option
           )
```

    [[1]]
    [1] The best treatment arm with probability of being better than control is treatment 1 with probability 0.44

    [[2]]
         Treatment Size
    [1,]         0   81
    [2,]         1   49
    [3,]         2   38
    [4,]         3   60

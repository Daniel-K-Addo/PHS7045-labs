Lab 2
================

``` r
source("lab2RelatedFunctions.R")
# allocationUpdate() Function: Compare treatment means with ctrl
# outcomeDefinition() Function: Update allocation probs
# treatmentComparison() Function: Update allocation probs
```

# Design 1

``` r
design_one <- function(N_samp = 228, # Total number of participants
                       trt_n = 4, # Number of Arms
                       trt_effect = rep(0.35,4), # treatment effect for each arm 
                       alpha = 0.35,  beta = 0.65, # prior
                       n_post = 1000 # number of posterior draws
                       ){
  # Equal allocation sample size determination
  design_option <- 1 # Specifying design option as needed later
  n_samp <- rep(N_samp/trt_n, trt_n)
  Y <- NULL
  post_y <- NULL
  for(i in 1:trt_n){
    # Definition of outcome given treatment effect within each arm
    Y[[i]] <- rbinom(n_samp[i],1,trt_effect[i])
    # Posterior draws
    post_y[[i]] <- rbeta(n_post, 
                         alpha + sum(Y[[i]]), 
                         beta + n_samp[i] - sum(Y[[i]])
                         )
  }
  # Compare treatments with control and select best treatment
  treatmentComparison(trt_n,post_y,n_samp,design_option)
}
```

# Testing the ‘design_one’ function

``` r
set.seed(4832)
design_one(N_samp = 228, # Total number of participants
           trt_n = 4, # Number of Arms
           trt_effect = rep(0.35,4), # treatment effect for each arm 
           alpha = 0.35,  beta = 0.65, # prior
           n_post = 1000 # number of posterior draws # set seed for replication
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

    [[3]]
    [1] "Trial was NOT successful"

# Design 2 (With the option of Design 1)

``` r
design_two <- function(N_samp = 228, # Total number of participants
                       trt_n = 4, # Number of Arms
                       trt_effect = rep(0.35,4), # treatment effect for each arm 
                       alpha = 0.35,  beta = 0.65, # prior
                       n_post = 1000, # number of posterior draws
                       allocation_size = 40, # Initial Sample to allocate
                       design_option = 2 # Design option 
                       ){
  # Condition to revert to design one:
  allocation_size <- ifelse(design_option==1, N_samp, allocation_size)
  # Compute # of interim allocation steps needed
  interim_steps <- ceiling(N_samp/allocation_size) 
  Y <- NULL # Output storage
  V_t_norm <- rep(1,trt_n)/trt_n
  
  for(j in 1:interim_steps){
    post_y <- NULL
    post_y_all <- NULL
    # Define allocation probabilities
    if(j==1){ # First time, allocate equally across treatments
      allocation_prob <- rep(1,trt_n)/trt_n
      n_allocate <- round(allocation_prob*allocation_size)
      n_allocate[trt_n] <- allocation_size - sum(n_allocate[-trt_n])
      n_samp <- n_allocate
    }else if(j!=interim_steps){ # Allocate wrt allocation probs across treatments
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
      Y[[i]] <- outcomeDefinition(i,j,Y,n_allocate,trt_effect)
      
      # Posterior draws from a beta distribution
      post_y[[i]] <- rbeta(n_post,
                           alpha + sum(Y[[i]]),
                           beta + n_samp[i] - sum(Y[[i]])
                           )
      post_y_all <- cbind(post_y_all,post_y[[i]])
    }
    
    # Update allocation probability after posterior draws
    V_t_norm <- allocationUpdate(post_y_all, n_samp)
  }
  
  # Compare treatments with control and select best treatment
  treatmentComparison(trt_n,post_y,n_samp,design_option)
  
}
```

# Testing the ‘design_two’ function

``` r
set.seed(4832)
design_two(N_samp = 228, # Total number of participants
           trt_n = 4, # Number of Arms
           trt_effect = rep(0.35,4), # treatment effect for each arm 
           alpha = 0.35,  beta = 0.65, # prior
           n_post = 1000, # number of posterior draws
           allocation_size = 40, # Initial Sample to allocate
           design_option = 2 # Design option
           )
```

    [[1]]
    [1] The best treatment arm with probability of being better than control is treatment 1 with probability 0.88

    [[2]]
         Treatment Size
    [1,]         0   76
    [2,]         1   48
    [3,]         2   53
    [4,]         3   51

    [[3]]
    [1] "Trial was NOT successful"

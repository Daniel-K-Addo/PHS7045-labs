Homework 1
================

# Homework 1 Submission

## Daniel Addo

# Due date

Tuesday, February 7

# Background

Response-adaptive randomization (RAR) has been used in precision
medicine trials, such as the trials
[I-SPY-2](https://www.nejm.org/doi/pdf/10.1056/NEJMoa1513750?articleTools=true),
[BATTLE-1](https://aacrjournals.org/cancerdiscovery/article/1/1/44/2198?casa_token=pK1gZcX-FgkAAAAA:KmsD6qnoaOMxqHJlg0VGlmqr2nqIl49Xupuh0FX7nnJXNjtdBwVWsdmVtUIXKdEWQ_e5i9pG),
and
[BATTLE-2](https://cdn.amegroups.cn/journals/amepc/files/journals/12/articles/6846/public/6846-PB2-R2.pdf)
to gather early evidence of treatment arms that work best for a given
biomarker. Throughout RAR, the treatment allocation adjusts depending on
which treatment arm looks most promising. RAR is criticized for the
following reasons
[(Korn)](https://watermark.silverchair.com/djx013.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAAvswggL3BgkqhkiG9w0BBwagggLoMIIC5AIBADCCAt0GCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMUKqYjuX4J38OrUAJAgEQgIICrkuWgqA03alHxi4xfUz3ybPTepY21z2PaOZFRSmRgKhEf4ROr5tIP8MoeLwYxIexRpjXLTjKQ_Yn6s2oYBYUTJEG_gVqeevFGwvxyi8zas8R1GyiQWDyjnhBsjKcAsKCl6M63_K-LXEPLj7nyw4FZEefXgMSO91jz_bKseNLmhDEmZ7oPvO_xlBzWptEW4gE8I7mrv8h3NbFrM0BrbDIeOkIr36F3X-F3ip5a8lEDirZwrKyLCb598ivER1JVlr7iCzNV9OYHv6P79E6YxKGxJPDNk1WJ-ClcUb6aA87-Gae81SVipvqhu2npbLCFtWVikZKliWQ5lPE9CieOB9hnd93HLsp7Da9U3D0jodTMbk9RE28I7kzuoWHO8BEoPK8p1_1nTWw6pV5CbS02RFxtTr4-aCMm364vJAHrgP3rNmvi-KEGB4rxufA_hebdi2yS20nll2HdAVT81m_cm87x3YvVP7Ut9H-0zTCAVYACBJJUjKS7w7QKkIN-FqdQ7N7u_YJ83bl69sMF0fddIVTA6WFAJWpVmiij5ou34auXuzDtEKLN5nmQ-467HZtpMAkoyFxcyCdtI_ieuyCp1jlP3IC07JZAWvA_8A2W8hEFaRBimq0STpclvxEgt2OCDtE9kTXvS8v6ArS4U6ZkuM7Uw3PSG1ISoYAWPPvWzGMKWU4W4nCS0rT-TPWPJkPf8TGkUK9Q1cDemuYlWZBrYN-RUJ8X204vbqa-p9jsRKIfsQ0QDrW97qHYxf-659kKav288WZ8d5wmESsLKyHEc8JatY2dvmsIMIdeK3mKA68qDUqS3C1yO7X0T4BJZpJPGjjPzFrrqaLAmLdhdqITE5AcT_Eiy3upRuYw3TdqbznbDog3k2K2yIgQUZwu2HC9q_fJhqmxh0WkHQDyYeiXyqb):

1.  Possible bias: Time trends in participant enrollment (example:
    healthier participants earlier) may lead to a bias in estimating
    which treatment is superior
2.  Possible inefficiency: Unequal allocation may lead to statistical
    inefficiencies compared to equal allocation
3.  Possibly unethical: A moderately large sample size could be enrolled
    onto an arm worse than control

It’s not always bad for a method to be criticized – it means there are
open questions to be addressed by new/improved methods. For example, the
manuscript [Comparison of methods for control allocation in multiple arm
studies using response adaptive
randomization](https://journals.sagepub.com/doi/pdf/10.1177/1740774519877836)
develops and compares methods to improve RAR efficiency compared to
equal allocation by maintaining a reasonable number of participants
allocated to the control arm.

# Assignment

Replicate the results in Table 2 of [Comparison of methods for control
allocation in multiple arm studies using response adaptive
randomization](https://journals.sagepub.com/doi/pdf/10.1177/1740774519877836)
for the designs RMatch and F25. Use only `base` R functions and use
functions to avoid duplicating multiple lines of code (i.e., this is
called modularizing your code).

In this assignment, you will practice writing functions, loops, and
using the \*apply functions.

# Guidance

1.  The manuscript uses a minimally informative, normal prior for the
    log-odds. Rather than this prior, use a Beta(0.35, 0.65) prior on
    the response rate for each arm. This is minimally informative
    favoring the null hypothesis and allows you to use the Beta-Binomial
    conjugacy to obtain a Beta posterior distribution. (Refer to [lab
    2](https://uofuepibio.github.io/PHS7045-advanced-programming/week-02-lab.html)
    for notation of the posterior distribution).

2.  A way to estimate $P_t$(Max) is to `cbind` K = \[1000+\] draws from
    the posterior distribution of each arm and to see how frequently
    (across the K draws from each arm) each arm is drawn to be the
    largest.

3.  The value of $\delta$ was found through simulation under the null
    scenario such that an efficacious trial was declared to be found
    only 2.5% of the time. The manuscript provides values for $\delta$
    though verify and modify as needed (there could be slight
    differences by using a different prior).

4.  The manuscript uses 100K replicates to determine $\delta$ and
    estimate the values in Table 2. Start with a small number of
    replicates (1K or 10K) to make sure the code is running correctly
    and your results are in the ball park of being similar to the
    manuscript. Ramp up the number of replicates as feasible (for this
    assignment feasible is a run time of no longer than 20 minutes).

5.  Please talk to us if you are having difficulties with this
    assignment. We realize it is in a space that may be new to you, and
    it is not the intent for this assignment to take more than 7-10
    hours over the course of 2 weeks.

# Solution

## Designs RMatch and F25 incorporated as one function

``` r
source("hw1RelatedFunctions.R") #allocationUpdate & treatmentComparison fxns

designRAR <- function(N_samp = 228, # Total number of participants
                       trt_n = 4, # Number of Arms
                       trt_effect = rep(0.35,4), # treatment effect for each arm 
                       alpha = 0.35,  beta = 0.65, # prior
                       n_post = 1000, # number of posterior draws
                       allocation_size = 40, # Initial Sample to allocate
                       design_option = "RMatch" # Design option: c("F25", "RMatch") 
                       ){
  # Condition to use F25 (Design 1):
  allocation_size <- ifelse(design_option=="F25", N_samp, allocation_size)
  allocation_prob <- rep(1,trt_n)/trt_n # Equal allocation
  
  # Definition of outcome possibilities given treatment effect within each arm
  Y0 <- do.call(cbind,lapply(1:4, function(x){
    rbinom(N_samp,1,trt_effect[x])
  }))
  
  # Compute # of interim allocation steps needed
  interim_steps <- ceiling(N_samp/allocation_size)
  for(j in 1:interim_steps){
    # Define allocation probabilities and randomly assign outcomes
    if(j==1){ # First time, allocate equally across treatments
      n_allocate <- round(allocation_prob*allocation_size)
      n_samp <-n_allocate # Note sample sizes
      Y <- lapply(1:4, function(x){# Randomly assign outcome respectively
        sample(Y0[,x],n_allocate[x])
      })
    }else{ # Allocate wrt allocation probs across treatments
      # Account for possibly reduced allocation size in final step
      allocation_size <- ifelse(j!=interim_steps, allocation_size, N_samp %% allocation_size)
      n_allocate <- c(1:4,sample(1:trt_n,allocation_size,replace = T, 
                                 prob = allocation_prob)) |> table()-1
      n_samp <-n_allocate+n_samp # Note updated sample sizes
      Y <- lapply(1:4, function(x){# Randomly assign outcome respectively
        c(Y[[x]],sample(Y0[,x],n_allocate[x]))}) # Update Y
    }
    
    # Posterior draws from a beta distribution
    post_y_all <- do.call(cbind,lapply(Y, function(x){
      rbeta(n_post, alpha + sum(x), 
            beta + length(x) - sum(x))
    }))
    # Update allocation probability after posterior draws
    allocation_prob <- allocationUpdate(post_y_all, n_samp)
  }
  # Compare treatments with control and select best treatment
  treatmentComparison(post_y_all)
}
```

## Trial Runs of designRAR

``` r
# Multiple trials of F25 design
set.seed(4832)
trackRecordF25 <- replicate(5e4, 
                            designRAR(N_samp = 228, # Total number of participants
                                     trt_n = 4, # Number of Arms
                                     trt_effect = c(0.35,.45,.55,.65), # treatment effect for each arm 
                                     alpha = 0.35,  beta = 0.65, # prior
                                     n_post = 1000, # number of posterior draws
                                     allocation_size = 40, # Initial Sample to allocate
                                     design_option = "F25" # Design option: c("F25", "RMatch")
                                     ))
# Compute proportions as in Table 2 of paper
propR25 <- prop.table(table(trackRecordF25)) |> rev() |> cumsum()

# Multiple trials of RMatch design
set.seed(4832)
trackRecordRMatch <- replicate(5e4, 
                          designRAR(N_samp = 228, # Total number of participants
                                   trt_n = 4, # Number of Arms
                                   trt_effect = c(0.35,.45,.55,.65), # treatment effect for each arm 
                                   alpha = 0.35,  beta = 0.65, # prior
                                   n_post = 1000, # number of posterior draws
                                   allocation_size = 40, # Initial Sample to allocate
                                   design_option = "RMatch" # Design option: c("F25", "RMatch")
                                   ))
# Compute proportions as in Table 2 of paper
propRMatch <- prop.table(table(trackRecordRMatch)) |> rev() |> cumsum()

# Table Prep as in Table 2 of paper 
trackRecordAll <- rbind.data.frame(c("F25",rev(propR25[1:3]*100)),
                                   c("RMatch",rev(propRMatch[1:3]*100)))
names(trackRecordAll) <- c("Design Type", "Pr(pick arm 1 or better)(%)",
                           "Pr(pick arm 2 or better)(%)",
                           "Pr(pick arm 3)(%)")
# Print table
print(trackRecordAll)
```

      Design Type Pr(pick arm 1 or better)(%) Pr(pick arm 2 or better)(%)
    1         F25                      99.974                      99.148
    2      RMatch                      99.966                      98.162
      Pr(pick arm 3)(%)
    1            85.578
    2            82.182

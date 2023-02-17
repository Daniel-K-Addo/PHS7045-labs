Lab 6
================

# Outline of HW 1 solution

- Assign initial values and generate an excess of outcomes for each
  treatment
- Write out function that generate draws from posterior, updates
  allocation probs, estimates max prob of trt being better than control,
  and prints sample sizes.
- Write out design one function with arguments needed for postdraws
- Write out design two function with arguments with necessary arguments
- Set-up initially to determine number of looks in the case of
  residuals/otherwise, etc.
- Utilize postdraws function as necessary.

# Part 2: Compare efficiency between submission and solution

``` r
# Compare design one to design two
bench::mark("Solution"=design1(y,h0=h0,h1=h1,h2=h2,h3=h3),
            "Submission"=designRAR(N_samp = 228,trt_n = 4,trt_effect = c(0.35,.45,.55,.65), 
                      alpha = 0.35,  beta = 0.65, n_post = 1000, allocation_size = 40, 
                      design_option = "F25" # Design option: c("F25", "RMatch")
                                   ),
           relative = TRUE, check = FALSE)
```

    # A tibble: 2 x 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 Solution    2.40   2.18      1         1.89      NaN
    2 Submission  1      1         2.07      1         Inf

``` r
bench::mark("Soultion"=design2(y,N=N,nInterim = 40,h0=h0,h1=h1,h2=h2,h3=h3),
            "Submission"=designRAR(N_samp = 228,trt_n = 4,trt_effect = c(0.35,.45,.55,.65), 
                      alpha = 0.35,  beta = 0.65, n_post = 1000, allocation_size = 40, 
                      design_option = "RAR" # Design option: c("F25", "RMatch")
                                   ),
           relative = TRUE, check = FALSE)
```

    # A tibble: 2 x 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 Soultion    1      1         2.89      1        1   
    2 Submission  3.71   3.01      1         2.02     4.81

# Part 3: Parallelization method

``` r
N_samp = 228
trt_n = 4
trt_effect = c(0.35,.45,.55,.65)
alpha = 0.35
beta = 0.65
n_post = 1000
allocation_size = 40
design_option = "RMatch" # Design option: c("F25", "RMatch")
df <- function(i) {
  designRAR(N_samp = 228, # Total number of participants
                                     trt_n = 4, # Number of Arms
                                     trt_effect = c(0.35,.45,.55,.65), # treatment effect for each arm 
                                     alpha = 0.35,  beta = 0.65, # prior
                                     n_post = 1000, # number of posterior draws
                                     allocation_size = 40, # Initial Sample to allocate
                                     design_option = "F25" # Design option: c("F25", "RMatch")
                                     )
}
    
cores <- parallel::detectCores()
cl <- parallel::makePSOCKcluster(cores) 
parallel::clusterExport(cl, list("allocationUpdate","treatmentComparison",
                                 "designRAR"))

bench::mark("parallel"=parallel::parSapply(cl,1:1e3,
                           FUN=df),
            "replicate"=replicate(1e3,df()),
            relative = TRUE, check = FALSE)
```

    Warning: Some expressions had a GC in every iteration; so filtering is disabled.

    # A tibble: 2 x 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 parallel    1      1         5.92        1       NaN
    2 replicate   5.92   5.92      1         656.      Inf

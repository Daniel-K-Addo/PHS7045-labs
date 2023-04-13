Lab 5
================

# Lab description

For this lab, we will create a function for propensity score matching.
The goal is simple: write out a C++ function with Rcpp and measure how
faster it is compared to the following R implementation:

``` r
ps_matchR <- function(x) {
  
  match_expected <- as.matrix(dist(x))
  diag(match_expected) <- .Machine$integer.max
  indices <- apply(match_expected, 1, which.min)
  
  list(
    match_id = as.integer(unname(indices)),
    match_x  = x[indices]
  )
  
}
```

## Question 1: Create a simple function

Use the following pseudo-code template to get started (modified):

``` rcpp
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List ps_match1(const NumericVector & x) {
  
  int n = x.size(); //...prepare the output (save space)...
  IntegerVector indices(n); //...it should be an integer vector indicating the id of the match...
  NumericVector values(n); //...and a numeric vector with the value of `x` for the match...
  
  for (int i=0; i < n; ++i) {
    
    int best_n=0;
    double best_dist = std::numeric_limits < double >::max();

    for (int j=0; j < n; ++j) {
      
      if (i==j)
        continue;
     
      double tmp_dist = abs(x[i]-x[j]);
      if (tmp_dist < best_dist) { // if (...the closests so far...)
        
        best_dist = tmp_dist; //     ...update the optimum...
        best_n = j; // ... note index
      }
      
    }
    indices[i] = best_n+1;
    values[i] = x[best_n];

  }
  
  return List:: create(
    _["match_id"] =  indices,
    _["match_x"] = values
  );
  
}
```

Run both functions:

``` r
set.seed(1231)
x<-runif(5);x
```

    [1] 0.467915119 0.007559486 0.756561814 0.212552784 0.039240952

``` r
ps_match1(x)
```

    $match_id
    [1] 4 5 1 5 2

    $match_x
    [1] 0.212552784 0.039240952 0.467915119 0.039240952 0.007559486

``` r
ps_matchR(x)
```

    $match_id
    [1] 4 5 1 5 2

    $match_x
    [1] 0.212552784 0.039240952 0.467915119 0.039240952 0.007559486

## Question 2: Things can be done faster

In the previous question, we have a double loop running twice over the
full set of observations. We need you to write the C++ so that the
computational complexity goes below n^2. (hint: Distance is symmetric)

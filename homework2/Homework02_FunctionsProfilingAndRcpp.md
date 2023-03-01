Homework 2
================

## Part 1: Vectorizing Code

The following functions can be written to be more efficient without
using parallel computing:

1.  This function generates a n x k dataset with all its entries
    distributed Poisson with mean lambda.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  # Draw randomly from a Poisson(lambda) n*k times
  rpois(n*k, lambda) |> matrix(nrow = 100, byrow = TRUE)
}

# Benchmarking
bench::mark(
  fun1(),
  fun1alt(), relative = TRUE, check = FALSE
)
```

    # A tibble: 2 × 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1()      17.2   22.0       1        62.4     3.46
    2 fun1alt()    1      1        18.8       1       1   

2.  Like before, speed up the following functions (it is OK to use
    StackOverflow)

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  x <- rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  apply(dat,1,cumsum) |> t()
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
bench::mark(
  fun1(dat),
  fun1alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1(dat)     5.95   7.66      1         196.     5.84
    2 fun1alt(dat)  1      1         8.36        1      1   

``` r
# Test for the second
bench::mark(
  fun2(dat),
  fun2alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(dat)     4.71   3.54      1         1         NaN
    2 fun2alt(dat)  1      1         3.70      5.31      Inf

3.  Find the column max (hint: Check out the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  colMax <- max.col(t(x))
  n <- ncol(x)
  out <-  rep(NA, n)
  for(i in 1:n){
    out[i] <- x[colMax[i],i]
  }
  out
}

# Benchmarking
bench::mark(
  fun2(x),
  fun2alt(x), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(x)     5.59   5.05      1         1        3.52
    2 fun2alt(x)  1      1         5.08      1.38     1   

## Part 2: Rcpp code

As we saw in the Rcpp week, vectorization may not be the best solution.
For this part, you must write a function using Rcpp that implements the
propensity score matching algorithm. You can use Week 5’s lab as a
starting point for the problem. Your C++ file should look something like
the following:

``` rcpp
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
 List psmatch(
   NumericVector pscores,
   LogicalVector is_treated
 )
 {
  /*... setup the problem creating the output...*/
  int n = static_cast<int>(is_treated.size());
  IntegerVector indices(n); 
  NumericVector values(n); 

  /*
  ... Implement your matching (start from Week 5's lab)...
  ... You have to consider that matches are done againts groups, i.e.,
      Treated (is_treated == true) must be matched to control
      (is_treated == false)
  */
  for (int i = 0; i < n; ++i) {
    // For each treated person, we want to find a matching control
    if (is_treated[i] == true) {
      
      int best_n = 0;
      double best_diff = std::numeric_limits < double >::max();
      
      for (int j = 0; j < n; ++j) {
      // controls being matched be ith treated subject to identify optimal match
      if (is_treated[j] == false) {
        
      // If it is lower, then update
      double diff = std::abs(pscores[i] - pscores[j]);
      if (diff < best_diff) {
        
        best_n = j;
        best_diff = diff;
        }
      }
      }

      values[i] = pscores[best_n]; // noting the matched prop score
      indices[i] = best_n + 1; // noting the matched control index, adjusted for R
    
}
    }

  // Returning
  return List::create(
    _["match_id"] = indices,
    _["match_pscore"] = values 
  );

 }
```

# Example

``` r
set.seed(324)
x <- runif(25) # pscore
y <- rbinom(25,1,0.4) |> as.logical() # is_treated
ans <- psmatch(x,y)

cbind.data.frame(pscores=x,is_treated=y,ans$match_id, ans$match_pscore)
```

          pscores is_treated ans$match_id ans$match_pscore
    1  0.72902036      FALSE            0       0.00000000
    2  0.42364850      FALSE            0       0.00000000
    3  0.57661580      FALSE            0       0.00000000
    4  0.09934177      FALSE            0       0.00000000
    5  0.70636940       TRUE            1       0.72902036
    6  0.60503700      FALSE            0       0.00000000
    7  0.71185461       TRUE            1       0.72902036
    8  0.06374409       TRUE            4       0.09934177
    9  0.26172285      FALSE            0       0.00000000
    10 0.48886708      FALSE            0       0.00000000
    11 0.88849635      FALSE            0       0.00000000
    12 0.83119269      FALSE            0       0.00000000
    13 0.89723020       TRUE           11       0.88849635
    14 0.57575825      FALSE            0       0.00000000
    15 0.28525065       TRUE           17       0.26906280
    16 0.44037311       TRUE            2       0.42364850
    17 0.26906280      FALSE            0       0.00000000
    18 0.74989936      FALSE            0       0.00000000
    19 0.16649712      FALSE            0       0.00000000
    20 0.73459477      FALSE            0       0.00000000
    21 0.33969277      FALSE            0       0.00000000
    22 0.19104529       TRUE           19       0.16649712
    23 0.63684770      FALSE            0       0.00000000
    24 0.96209461       TRUE           11       0.88849635
    25 0.37565668       TRUE           21       0.33969277

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

/***R
ps_matchR <- function(x) {
  
  match_expected <- as.matrix(dist(x))
  diag(match_expected) <- .Machine$integer.max
  indices <- apply(match_expected, 1, which.min)
  
  list(
    match_id = as.integer(unname(indices)),
    match_x  = x[indices]
  )
  
}

set.seed(1231)
x<-runif(5)
ps_match1(x)

*/
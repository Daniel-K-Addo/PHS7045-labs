#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector add_vectors(NumericVector x, NumericVector y) {
  
  if (x.size()!=y.size())
    stop("Vector lengths must match");
  
  NumericVector ans(x.size());
  for(int i=0; i < x.size(); ++i){
    ans[i] = x[i]+y[i];
  }
  return ans;
}

/***R
add_vectors(1:5,6:10)
add_vectors(1:5,6)
*/
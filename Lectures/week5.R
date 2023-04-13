Rcpp::cppFunction('
int myfun(const NumericVector & x)
{
  int i = 0;
  for (auto & x_ : x)
    Rprintf("The value of x[%i] is: %.2f\\n", i++, x_);
  
  return 0;
}')

myfun(c(1, .12, 3))
#> The value of x[0] is: 1.00
#> The value of x[1] is: 0.12
#> The value of x[2] is: 3.00
#> [1] 0


# Week 5
fibR <- function(n) {
  if (n <= 1)
    return(n)
  fibR(n - 1) + fibR(n - 2)
}

# Is it working?
c(
  fibR(0), fibR(1), fibR(2),
  fibR(3), fibR(4), fibR(5),
  fibR(6)
)

# Writing with RCPP
Rcpp::cppFunction('
int fibCpp(int n){
  
  if (n <= 1)
   return(n);
  
  return fibCpp(n-1) + fibCpp(n-2);
}')

fibCpp(50)

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector diffC(NumericVector x, int lag = 1,
                    bool na_rm = false) {
  int n = x.size();
  
  if (lag >= n) stop("`lag` must be less than `length(x)`.");
  
  NumericVector out(n - lag);
  
  for (int i = lag; i < n; i++) {
    if (NumericVector::is_na(x[i]) ||
        NumericVector::is_na(x[i - lag])) {
      if (!na_rm) {
        return rep(NumericVector::create(NA_REAL), n - lag);
      }
      out[i - lag] = NA_REAL;
      continue;
    }
    out[i - lag] = x[i] - x[i - lag];
  }
  
  return out;
}
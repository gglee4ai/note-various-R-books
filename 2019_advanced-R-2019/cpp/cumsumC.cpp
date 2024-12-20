#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cumsumC(NumericVector x, bool na_rm = false) {
  int n = x.size();
  NumericVector out(n);
  LogicalVector is_missing = is_na(x);
  
  if (!na_rm) {
    out[0] = x[0];
    for (int i = 1; i < n; ++i) {
      if (is_missing[i - 1]) {
        out[i] = NA_REAL;
      } else{
        out[i] = out[i - 1] + x[i];
      }
    }
  }
  
  if (na_rm) {
    if (is_missing[0]) {
      out[0] = 0;
    } else {
      out[0] = x[0];
    } 
    for (int i = 1; i < n; ++i) {
      if (is_missing[i]) {
        out[i] = out[i-1] + 0;
      } else {
        out[i] = out[i-1] + x[i];
      } 
    }
  }
  
  return out;
}
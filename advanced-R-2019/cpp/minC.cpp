#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector minC(NumericVector x, bool na_rm = false) {
  int n = x.size();
  NumericVector out = NumericVector::create(R_PosInf);
  
  if (na_rm) {
    for (int i = 0; i < n; ++i) {
      if (x[i] == NA_REAL) {
        continue;
      }
      if (x[i] < out[0]) {
        out[0] = x[i];
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      if (NumericVector::is_na(x[i])) {
        out[0] = NA_REAL;
        return out;
      }
      if (x[i] < out[0]) {
        out[0] = x[i];
      }
    }
  }
  
  return out;
}
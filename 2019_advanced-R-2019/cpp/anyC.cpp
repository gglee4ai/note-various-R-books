#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector anyC(LogicalVector x, bool na_rm = false) {
  int n = x.size();
  LogicalVector out = LogicalVector::create(false);
  
  if (na_rm == false) {
    for (int i = 0; i < n; ++i) {
      if (LogicalVector::is_na(x[i])) {
        out[0] = NA_LOGICAL;
        return out;
      } else {
        if (x[i]) {
          out[0] = true;
        }
      }
    }
  }
  
  if (na_rm) {
    for (int i = 0; i < n; ++i) {
      if (LogicalVector::is_na(x[i])) {
        continue;
      }
      if (x[i]) {
        out[0] = true;
        return out;
      }
    }
  }
  
  return out;
}
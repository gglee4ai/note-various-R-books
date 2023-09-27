#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector uniqueC(NumericVector x) {
  std::unordered_set<int> seen;
  int n = x.size();
  
  std::vector<double> out;
  for (int i = 0; i < n; ++i) {
    if (seen.insert(x[i]).second) out.push_back(x[i]);
  }
  
  return wrap(out);
}


// As a one-liner
// [[Rcpp::export]]
std::unordered_set<double> uniqueCC(NumericVector x) {
  return std::unordered_set<double>(x.begin(), x.end());
}
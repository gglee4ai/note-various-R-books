#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector inC(CharacterVector x, CharacterVector table) {
  std::unordered_set<String> seen;
  seen.insert(table.begin(), table.end());
  
  int n = x.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = seen.find(x[i]) != seen.end();
  }
  
  return out;
}
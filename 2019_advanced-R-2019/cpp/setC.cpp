#include <Rcpp.h>
#include <unordered_set>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
IntegerVector unionC(IntegerVector x, IntegerVector y) {
  int nx = x.size();
  int ny = y.size();
  
  IntegerVector tmp(nx + ny);
  
  std::sort(x.begin(), x.end()); // unique
  std::sort(y.begin(), y.end());
  
  IntegerVector::iterator out_end = std::set_union(
    x.begin(), x.end(), y.begin(), y.end(), tmp.begin()
  );
  
  int prev_value = 0;
  IntegerVector out;
  for (IntegerVector::iterator it = tmp.begin();
       it != out_end; ++it) {
    if ((it != tmp.begin())  && (prev_value == *it)) continue;
    
    out.push_back(*it);
    
    prev_value = *it;
  }
  
  return out;
}

// [[Rcpp::export]]
IntegerVector intersectC(IntegerVector x, IntegerVector y) {
  int nx = x.size();
  int ny = y.size();
  
  IntegerVector tmp(std::min(nx, ny));
  
  std::sort(x.begin(), x.end());
  std::sort(y.begin(), y.end());
  
  IntegerVector::iterator out_end = std::set_intersection(
    x.begin(), x.end(), y.begin(), y.end(), tmp.begin()
  );
  
  int prev_value = 0;  
  IntegerVector out;
  for (IntegerVector::iterator it = tmp.begin();
       it != out_end; ++it) {
    if ((it != tmp.begin()) && (prev_value == *it)) continue;
    
    out.push_back(*it);
    
    prev_value = *it;
  }
  
  return out;
}

// [[Rcpp::export]]
IntegerVector setdiffC(IntegerVector x, IntegerVector y) {
  int nx = x.size();
  int ny = y.size();
  
  IntegerVector tmp(nx);
  
  std::sort(x.begin(), x.end());
  
  int prev_value = 0;
  IntegerVector x_dedup;
  for (IntegerVector::iterator it = x.begin();
       it != x.end(); ++it) {
    if ((it != x.begin()) && (prev_value == *it)) continue;
    
    x_dedup.push_back(*it);
    
    prev_value = *it;
  }
  
  std::sort(y.begin(), y.end());
  
  IntegerVector::iterator out_end = std::set_difference(
    x_dedup.begin(), x_dedup.end(), y.begin(), y.end(), tmp.begin()
  );
  
  IntegerVector out;
  for (IntegerVector::iterator it = tmp.begin();
       it != out_end; ++it) {
    out.push_back(*it);
  }
  
  return out;
}
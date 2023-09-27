# C10csderivex.R

if ("package:pracma" %in% search()) {
   detach(package:pracma) # avoid confusion between two grad() functions
 }

 f1 <- function(x) 1 + x^2
 f2 <- function(x) abs(1 + x^2)
 require(numDeriv)
 grad(f1, 1.0, method = "complex")
 grad(f2, 1.0, method = "complex")

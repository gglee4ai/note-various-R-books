## ----label=C07nleqslv-ex1, include=TRUE, cache=TRUE----------------------
# Dennis Schnabel example 6.5.1 page 149
dslnex <- function(x) {
r <- numeric(2)
r[1] <- x[1]^2 + x[2]^2 - 2
r[2] <- exp(x[1]-1) + x[2]^3 - 2
r
}
jacdsln <- function(x) {
n <- length(x)
Df <- matrix(numeric(n*n),n,n)
Df[1,1] <- 2*x[1]
Df[1,2] <- 2*x[2]
Df[2,1] <- exp(x[1]-1)
Df[2,2] <- 3*x[2]^2
Df
}
ssdsln<-function(x) {
  ## a sum of squares function for 6.5.1 example
   rr<-dslnex(x)
   val<-as.double(crossprod(rr))
}

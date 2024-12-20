## ----label=C09rbtrust, echo=TRUE, cache=TRUE-----------------------------
frh <- function(x) {   ## Rosenbrock Banana function gradient
    x1 <- x[1]
    x2 <- x[2]
    h11 <- -400*x2 +1200*x1*x1 + 2
    h12 <- -400*x1
    h21 <- h12
    h22 <- 200
    HH <- matrix(c(h11, h12, h21, h22), nrow=2, ncol=2)
}
objfun1<-function(x){
   val<-fr(x)
   gg<-frg(x)
   HH<-frh(x)
   list(value=val, gradient=gg, hessian=HH)
}
objfun2 <- function(x) {
         stopifnot(is.numeric(x))
         stopifnot(length(x) == 2)
         f <- expression(100 * (x2 - x1^2)^2 + (1 - x1)^2)
         g1 <- D(f, "x1")
         g2 <- D(f, "x2")
         h11 <- D(g1, "x1")
         h12 <- D(g1, "x2")
         h22 <- D(g2, "x2")
         x1 <- x[1]
         x2 <- x[2]
         f <- eval(f)
         g <- c(eval(g1), eval(g2))
         B <- rbind(c(eval(h11), eval(h12)), c(eval(h12), eval(h22)))
         list(value = f, gradient = g, hessian = B)
}
require(trust)
atrust1<-trust(objfun1, sstart, rinit=1, rmax=5)
atrust1
## Get same answers from 
## atrust2<-trust(objfun2, sstart, rinit=1, rmax=5)

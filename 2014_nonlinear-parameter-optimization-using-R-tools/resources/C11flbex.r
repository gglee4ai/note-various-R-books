## ----label=C11flb1, echo=TRUE, cache=TRUE--------------------------------
require(optimx)
flb <- function(x) { 
     p <- length(x)
     sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) 
}
start<-rep(2,4)
lo<-rep(2,4)
up<-rep(4,4)
ans4a<-try(optimx(start, fn=flb, method=c("nmkb"), lower=lo, upper=up))



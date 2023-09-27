## ----label=C11flb3, echo=TRUE, cache=TRUE--------------------------------
require(optimx)
flb <- function(x) { # include this again so script is self-standing
     p <- length(x)
     sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) 
}
start<-rep(2,4)
lo<-rep(2,4)
up<-rep(4,4)
ans4<-optimx(start, fn=flb, method=c("Rvmmin","L-BFGS-B","bobyqa"), lower=lo, upper=up,
       control=list(usenumDeriv=TRUE))
print(summary(ans4))


## ----label=C11boundchk1, echo=TRUE, cache=TRUE---------------------------
require(optextras)
## extract first solution from ans4
ans41<-ans4[1,]
c41<-coef(ans41) ## get coefficients
bdc<-bmchk(c41, lower=lo, upper=up, trace=1)
print(bdc$bchar)
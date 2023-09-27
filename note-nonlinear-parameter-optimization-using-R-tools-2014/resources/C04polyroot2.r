## ----label=C04chunk11, echo=TRUE, size="scriptsize", cache=TRUE----------
z <- c(16, -8, 1)
simpol2<-function(x){ # calculate polynomial z at x 
   val <- (x-4)^2
}
tint<-c(-5,5)
cat("roots of polynomial specified by ")
print(z)
require(polynom)
allroots<-polyroot(z)
print(allroots)
cat("single root from uniroot on interval ",tint[1],",",tint[2],"\n")
rt1<-try(uniroot(simpol2, tint), silent=TRUE)
print(strwrap(rt1))
cat("\n Try a cubic\n")
cub<-function(z) {val<-(z-4)^3}
cc<-c(-64, 48, -12, 1)
croot<-polyroot(cc)
croot
ans<-uniroot(cub, lower=2, upper=6)
ans

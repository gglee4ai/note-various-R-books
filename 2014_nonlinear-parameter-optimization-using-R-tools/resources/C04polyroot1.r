## ----label=C04chunk10, echo=TRUE, size="scriptsize", cache=TRUE----------
z <- c(10, -3, 0, 1)
simpol<-function(x){ # calculate polynomial z at x 
   zz <- c(10, -3, 0, 1)
   ndeg<-length(zz)-1 # degree of polynomial
   val<-zz[ndeg+1]
   for (i in 1:ndeg){
       val<-val*x+zz[ndeg+1-i]
   }
   val
}
tint<-c(-5,5)
cat("roots of polynomial specified by ")
print(z)
require(polynom)
allroots<-polyroot(z)
print(allroots)
cat("single root from uniroot on interval ",tint[1],",",tint[2],"\n")
rt1<-uniroot(simpol, tint)
print(rt1)

## ----label=C02newt1, echo=TRUE, cache=TRUE-------------------------------
F<-function(x)  exp(-.05*x)*(x-4)^2
curve(F, from=0, to=5)
newt<-function(x){
  xnew<-x - ( exp(-0.05 * x) * (2 * (x - 4)) - exp(-0.05 * x) * 0.05 * 
         (x - 4)^2) / (exp(-0.05 * x) * 2
        - exp(-0.05 * x) * 0.05 * (2 * (x - 4)) - 
       (exp(-0.05 * x) * 0.05 * (2 * (x - 4)) - exp(-0.05 * x) * 
         0.05 * 0.05 * (x - 4)^2))
}
x<-1
xold<-0
while (xold != x) {
   xold<-x
   cat("f(",x,")=", F(x),"\n")
   x<-newt(xold)
}

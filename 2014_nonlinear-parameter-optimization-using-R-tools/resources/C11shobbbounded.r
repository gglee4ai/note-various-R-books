## ----label=C11shobbbd1, echo=TRUE, cache=TRUE----------------------------
require(nlmrt)
source("C11lhobbs.R", echo=FALSE) # to provide the function and gradient
y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 38.558, 
      50.156, 62.948, 75.995, 91.972)
t<-1:12
wdata<-data.frame(y=y, t=t)
anlxb<-nlxb(y ~ 100*asym/(1+10*strt*exp(-0.1*rate*t)), 
            start=c(asym=2,strt=2,rate=2), lower=c(2,2,2), 
            upper=c(4,4,4), data=wdata, trace=FALSE)
print(anlxb)
# Get unconstrained Jacobian and its singular values
# First create the function (you can display it if you wish)
myformula<-y ~ 100*asym/(1+10*strt*exp(-0.1*rate*t))
# The parameter vector values are unimportant; the names are needed
mj<-model2jacfun(myformula, pvec=coef(anlxb))
# Evaluate at the solution parameters
mjval<-mj(prm=coef(anlxb), y=y, t=t)
# and display the singular values
print(svd(mjval)$d)
# Create the sumsquares function
mss<-model2ssfun(myformula, pvec=coef(anlxb))
# Evaluate gradient at the solution
require(numDeriv)
mgrval<-grad(mss, x=coef(anlxb), y=y, t=t)
print(mgrval)
# Evaluate the hessian at the solution (x rather than prm!)
mhesssval<-hessian(mss, x=coef(anlxb), y=y, t=t)
print(eigen(mhesssval)$values)

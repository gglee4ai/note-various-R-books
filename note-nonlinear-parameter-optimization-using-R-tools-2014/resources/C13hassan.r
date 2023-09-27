## ----label=C13hassan0, echo=TRUE-----------------------------------------
hn<-read.csv("hassan182.csv",header=TRUE)
convi<-function(x, dta){ # dta not used
    h<-rep(NA,1) # vector value
    h[1]<- x[3]*x[6]-x[4]*x[5]
    h
}
uncmod<-lm(one~two+three+four+five+six, data=hn)
uncmod
bu<-coef(uncmod)
cat("Constraint violation = b3*b6-b4*b5 =",as.numeric(convi(bu)),"\n")
cat("Sum of squared residuals = ",as.numeric(crossprod(uncmod$residuals)),"\n")


## ----label=C13hassan0b, echo=TRUE----------------------------------------
uncmod2<-nls(one ~ b1 + two*b2 + three*b3 + four*b4 +five*b5 + six*b6, data=hn)
summary(uncmod2)

tmp<-readline("continue")

## ----label=C13hassan1, echo=TRUE-----------------------------------------
res3<-function(x, dta) {
  b1<-x[1]
  b2<-x[2]
#  b3<-x[3]
  b4<-x[3]
  b5<-x[4]
  b6<-x[5]
  res<- dta$two*b2 + dta$three*b4*b5/b6 + dta$four*b4 +dta$five*b5 + dta$six*b6 + b1 - dta$one
  return(-res)
}
ss3<-function(x,dta) {
    res<-res3(x,dta)
    sum(res*res)
}
require(nlmrt)
require(optimx)
st3<-setNames(bu[-3], c("b1", "b2", "b4", "b5", "b6"))
anlfb3<-try(nlfb(start=st3, resfn=res3, dta=hn))
if (class(anlfb3) != "try-error") {
        cnb3<-coef(anlfb3)
	print(cnb3)
        cat("RSS=",crossprod(anlfb3$resid),"\n") 
} else { cat("Error - failed\n") }
## derived_b3
cnb3["b4"]*cnb3["b5"]/cnb3["b6"]
##
## Try optimx 
aop3<-optimx(st3, fn=ss3, gr="grnd", dta=hn, control=list(all.methods=TRUE))
summary(aop3, order=value)
tmp<-readline("continue")



## ----label=C13hassan2, echo=TRUE-----------------------------------------
resall<-function(x, dta) {
  b1<-x[1]
  b2<-x[2]
  b3<-x[3]
  b4<-x[4]
  b5<-x[5]
  b6<-x[6]
  res<- dta$two*b2 + dta$three*b3 + dta$four*b4 +dta$five*b5 + dta$six*b6 + b1 - dta$one
  return(-res)
}
jacall<-function(x, dta) {
  b1<-x[1]
  b2<-x[2]
  b3<-x[3]
  b4<-x[4]
  b5<-x[5]
  b6<-x[6]
  nobs<-dim(dta)[1]
  JJ<-cbind(rep(-1,nobs), -dta$two, -dta$three, -dta$four, -dta$five, -dta$six)
}
ssall<-function(x,dta) {
    res<-resall(x,dta)
    sum(res*res)
}

stones<-rep(1,6) # trivial start
names(stones)<-c("b1", "b2", "b3", "b4", "b5", "b6")
stunc<-coef(uncmod) # unconstrained parameter start
print(stunc)
tmp<-readline("continue")



## ----label=C13cocall, echo=TRUE, eval=FALSE------------------------------
## require(alabama)
## aalabco<-constrOptim.nl(par=stones, fn=ssall, heq=convi, dta=hn)
## aauglag<-auglag(stones, fn=ssall, heq=convi, dta=hn)
## require(Rsolnp)
## asolnp<-solnp(stones, fun=ssall, eqfun=convi, eqB=c(0), dta=hn)


## ----label=C13corun, echo=FALSE------------------------------------------
require(alabama)
cat("Start from all ones -- constrOptim.nl\n")
aalabco<-constrOptim.nl(par=stones, fn=ssall, heq=convi, dta=hn, control.outer=list(trace=FALSE))
aalabco$par
cat("constrOptim.nl: sumsquares =",ssall(aalabco$par, dta=hn),
        "   constr. violation=",convi(aalabco$par),"\n")
cat("\n")
cat("Start from unconstrained model -- constrOptim.nl\n")
aalabcu<-constrOptim.nl(par=stunc, fn=ssall, heq=convi, dta=hn, control.outer=list(trace=FALSE))
aalabcu$par
cat("constrOptim.nl: sumsquares =",ssall(aalabcu$par, dta=hn),
        "   constr. violation=",convi(aalabcu$par),"\n\n")
cat("\n")
tmp<-readline("continue")



cat("Start from all ones -- auglag\n")
aauglo<-auglag(stones, fn=ssall, heq=convi, dta=hn, control.outer=list(trace=FALSE))
aauglo$par
cat("auglag: sumsquares =",ssall(aauglo$par, dta=hn),
        "   constr. violation=",convi(aauglo$par),"\n")
cat("\n")

cat("Start from unconstrained model -- auglag\n")
aauglu<-auglag(stunc, fn=ssall, heq=convi, dta=hn, control.outer=list(trace=FALSE))
aauglu$par
cat("auglag: sumsquares =",ssall(aauglu$par, dta=hn),
        "   constr. violation=",convi(aauglu$par),"\n\n")
cat("\n")
tmp<-readline("continue")


require(Rsolnp)
cat("Start from all ones -- solnp\n")
asolnpo<-solnp(stones, fun=ssall, eqfun=convi, eqB=c(0), dta=hn, control=list(trace=0))
asolnpo$pars
cat("solnp: sumsquares =",ssall(asolnpo$pars, dta=hn),
        "   constr. violation=",convi(asolnpo$pars),"\n\n")


cat("Start from unconstrained model -- solnp\n")
asolnpu<-solnp(stunc, fun=ssall, eqfun=convi, eqB=c(0), dta=hn, control=list(trace=0))
asolnpu$pars
cat("solnp: sumsquares =",ssall(asolnpu$pars, dta=hn),
        "   constr. violation=",convi(asolnpu$pars),"\n")
tmp<-readline("continue")



## ----label=C13hassan3, echo=TRUE-----------------------------------------
hassanp.f<-function(x, pen=100, dta){
   mod<-x[1]+x[2]*dta$two+x[3]*dta$three+x[4]*dta$four+x[5]*dta$five+x[6]*dta$six
   res<-mod-dta$one
   pfn<-x[3]*x[6] - x[4]*x[5]
   fun<-sum(res*res)+pen*pfn*pfn # return(fun)
}
xx<-coef(uncmod)
bestRSS<-1e300
rss<-1e299 # make it different from bestRSS to start loop
pen<-10
while (rss != bestRSS) {
    bestRSS<-rss
    pen=10*pen
    anmp<-optim(par=xx, fn=hassanp.f, dta=hn, pen=pen)
    xx<-anmp$par # for next iteration
    rss<-ssall(xx, dta=hn)
    cat("RSS = ",rss,"  for pen=",pen,"\n")
    cat("    Constraint violation =",convi(xx,dta=hn),"\n")
}
## Best parameters
print(xx)

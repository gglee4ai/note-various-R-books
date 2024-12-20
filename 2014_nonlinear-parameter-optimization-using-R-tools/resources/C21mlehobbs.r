## ----label=C21mle, echo=TRUE---------------------------------------------
require(stats4, quietly=TRUE)
lhobbs.res<-function(xl,y){ # log scaled Hobbs weeds problem -- residual
# base parameters on log(x)
    x<-exp(xl)
    if (abs(12*x[3]) > 50) { # check computability
       rbad<-rep(.Machine$double.xmax, length(x))
       return(rbad)
    }
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    t<-1:length(y)
    res<-x[1]/(1+x[2]*exp(-x[3]*t)) - y
}
lhobbs.lik<-function(Asym, b2, b3, lsig){ # likelihood function including sigma
  y <- c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443)
  y <- c(y, 38.558, 50.156, 62.948, 75.995, 91.972)
  xl<-c(Asym, b2, b3)
  logSigma<-lsig
  sigma2=exp(2.0*logSigma);
  res<-lhobbs.res(xl,y)
  nll<-0.5*(length(res)*log(2*pi*sigma2)+sum(res*res)/sigma2)
}
mystart<-list(Asym=1,b2=1,b3=1,lsig=1) # must be a list
amlef<-mle(lhobbs.lik, start=mystart, fixed=list(lsig=log(0.4)))
amlef
amlef@min # minimal neg log likelihood
amle<-mle(lhobbs.lik, start=as.list(coef(amlef)))
amle
val<-do.call(lhobbs.lik, args=as.list(coef(amle)))
val
# Note: This does not work
testval<-try(print(lhobbs.lik(as.list(coef(amle)))))
# But this displays the minimum of the negative log likelihood
amle@min


## ----label=C21mle2, echo=TRUE--------------------------------------------
require(bbmle, quietly=TRUE)
lhobbs2.lik<-function(Asym, b2, b3, lsig, y){ # likelihood function including sigma
  xl<-c(Asym, b2, b3)
  logSigma<-lsig
  sigma2=exp(2.0*logSigma);
  res<-lhobbs.res(xl,y)
  nll<-0.5*(length(res)*log(2*pi*sigma2)+sum(res*res)/sigma2)
}
y0 <- c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443)
y0 <- c(y0, 38.558, 50.156, 62.948, 75.995, 91.972)
mystart<-list(Asym=1,b2=1,b3=1,lsig=1) # must be a list
flist <- list(lsig=log(0.4))
amle2f<-mle2(lhobbs2.lik, start=mystart, data=list(y=y0), fixed=flist)
amle2f
amle2<-mle2(lhobbs2.lik, start=as.list(coef(amlef)), data=list(y=y0))
amle2


## ----label=C21mle2value, echo=TRUE---------------------------------------
amle@details$value


## ----label=C21maxlik, echo=TRUE------------------------------------------
suppressMessages(require(maxLik))
llh <- function(xaug, y){
   Asym<-xaug[1]
   b2<-xaug[2]
   b3<-xaug[3]
   lsig<-xaug[4]
   val<- (-1)*lhobbs2.lik(Asym, b2, b3, lsig, y)
}
aml<-maxLik(llh, start=c(1,1,1,1), y=y0)
aml

## ----C21nnlsa, echo=TRUE-------------------------------------------------
Sig1<-c( 0, 0, 1, 2, 3, 4, 3, 2, 1, 0, 0, 2, 4, 8, 16, 8, 5, 1, 0)
Sig2<-c( 2, 3, 4, 2, 1, 1, 3, 5, 7, 1, 0, 0, 0, 0, 0, 0, 5, 0, 0)
Sig3<-c( 0, 0, 0, 0, 0, 4, 4, 4, 1, 0, 0, 1, 14, 18, 16, 18, 15, 1, 10)

C<-cbind(Sig1, Sig2, Sig3)

bb<-C %*% as.matrix(c(0.23, 0.4, 0.1))

scale<- 0.15
require(setRNG)
setRNG(kind="Wichmann-Hill", seed=c(979,1479,1542), normal.kind="Box-Muller")
d <- bb + scale*rnorm(19)


## ----C21nnlsb, echo=TRUE-------------------------------------------------
require(nnls, quietly=TRUE)
aCd <- nnls(C, d)
aCd


## ----C21nnlsc, echo=TRUE-------------------------------------------------
############# another example ############
resfn <- function(x, matvec, A){
   x<-as.matrix(x)
   res <- (A %*% x) - matvec
}

jacfn <- function(x, matvec=NULL, A){
    A
}

ssfun<-function(x, matvec, A) {
   rr<-resfn(x, matvec, A)
   val<-sum(rr^2)
}
ggfun<-function(x, matvec, A) {
   rr<-resfn(x, matvec, A)
   JJ<-jacfn(x, matvec, A)
   gg<- 2*as.numeric(t(JJ) %*% rr)
}


## ----label=C21nnlsc2, echo=TRUE, eval=FALSE------------------------------
## # Check functions:
##  xx<-rep(1,3)
##  resfn
##  print(resfn(xx, d, C))
##  jacfn
##  print(jacfn(xx, d, C))
##  ssfun
##  print(ssfun(xx, d, C))
##  ggfun:
##  print(ggfun(xx, d, C))


## ----C21nnlsd, echo=TRUE-------------------------------------------------
require(nlmrt, quietly=TRUE)
strt <- c(p1=0, p2=0, p3=0)
aCdnlfb<-nlfb(strt, resfn, jacfn, lower=0, matvec=d, A=C)
aCdnlfb


## ----C21nnlse, echo=TRUE-------------------------------------------------
require(optimx, quietly=TRUE)
strt <- c(p1=0, p2=0, p3=0)
strt2 <- c(p1=0.1, p2=0.1, p3=0.1)
lo <- c(0,0,0)
aop<-optimx(strt, ssfun, ggfun, method="all", lower=lo, matvec=d, A=C)
summary(aop, order=value)
aop2<-optimx(strt2, ssfun, ggfun, method="all", lower=lo, matvec=d, A=C)
summary(aop2, order=value)


## ----C21nnlse2, echo=TRUE, eval=FALSE------------------------------------
## ## No gradient function is needed -- example not run
## ## aopn<-optimx(strt, ssfun, method="all", control=list(trace=0),
## ##       lower=c(0,0,0), matvec=d, A=C)
## ## summary(aopn, order=value)
## ## aop2n<-optimx(strt1, ssfun, method="all", lower=lo, matvec=d, A=C)
## ## summary(aop2n, order=value)

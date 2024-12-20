## ----label=C18raymat0, echo=FALSE, cache=FALSE---------------------------
rm(list=ls()) # cleanup


## ----label=C18raymat1, echo=TRUE-----------------------------------------
molermat<-function(n){
   A<-matrix(NA, nrow=n, ncol=n)
   for (i in 1:n){
      for (j in 1:n) {
          if (i == j) A[i,i]<-i
          else A[i,j]<-min(i,j) - 2
      }
   }
   A
}

rayq<-function(x, A){
   rayquo<-as.numeric(crossprod(x, crossprod(A,x))/crossprod(x))
}


## ----label=C18raymat2, echo=TRUE, eval=FALSE-----------------------------
 require(microbenchmark)
 require(compiler)
 molermatc<-cmpfun(molermat)
 rayqc<-cmpfun(rayq)
 n<-200
 x<-rep(1,n)
 tbuild<-microbenchmark(A<-molermat(n))$time
 tbuildc<-microbenchmark(A<-molermatc(n))$time
 teigen<-microbenchmark(evs<-eigen(A))$time
 tr<-microbenchmark(rayq(x, A))$time
 trc<-microbenchmark(rayqc(x, A))$time
 eigvec<-evs$vectors[,1] # select first eigenvector
 # normalize first eigenvector
 eigvec<-sign(eigvec[[1]])*eigvec/sqrt(as.numeric(crossprod(eigvec)))


## ----label=C18raycg1a, echo=TRUE, eval=FALSE-----------------------------
 require(optimx)
 tcg<-system.time(amax<-optimx(x, fn=rayq, method="Rcgmin",
       control=list(usenumDeriv=TRUE, kkt=FALSE), A=-A))[[3]]
 # summary(amax)
 cat("maximal eigensolution: Value=",-amax$value,"  timecg= ",tcg,"\n")
 tvec<-coef(amax)[1,]
 rcgvec<-sign(tvec)[[1]]*(tvec)/sqrt(as.numeric(crossprod(tvec)))
 cat("Compare with eigen()\n")
 cat("Difference in eigenvalues = ",(-amax$value[1]-evs$values[[1]]),"\n")
 cat("Max abs vector difference = ", max(abs(eigvec-rcgvec)),"\n")


## ----C18axmfcode, echo=TRUE, eval=FALSE----------------------------------
 axmoler <- function(x) {
 # A memory-saving version of A%*%x
 # For Moler matrix.
 n <- length(x)
 j <- 1:n
 ax <- rep(0, n)
 for (i in 1:n) {
   term <- x * (pmin(i, j) - 2)
   ax[i] <- sum(term[-i])
 }
 ax <- ax + j*x
 ax
 }
 require(compiler)
 require(microbenchmark)
 n<-200
 x<-rep(1,n)
 tax<-microbenchmark(ax<-axmoler(x))$time
 axmolerc<-cmpfun(axmoler)
 taxc<-microbenchmark(axc<-axmolerc(x))$time
 cat("time.ax, time.axc:",mean(tax), mean(taxc),"\n")
 ## On J6 gives time.ax, time.axc: 5591866 4925221


## ----label=C18raycg2, echo=TRUE------------------------------------------
dyn.load("/home/john/nlpor/supportdocs/tuning/rqmoler.so")
cat("Is the Fortran RQ function loaded? ",is.loaded('rqmoler'),"\n")


## ----label=C18raycg2x, echo=TRUE, eval=FALSE-----------------------------
 frqmoler<-function(x){
     n<-length(x)
     rq<-0.0
     res<-.Fortran("rqmoler", n=as.integer(n), x=as.double(x), rq=as.double(rq))
     res$rq
 }
 nfrqmoler<-function(x) { (-1)*frqmoler(x) }
 tcgrf<-system.time(amaxi<-optimx(x, fn=nfrqmoler, method="Rcgmin",
       control=list(usenumDeriv=TRUE, kkt=FALSE, trace=1)))[[3]]
 summary(tcgrf)

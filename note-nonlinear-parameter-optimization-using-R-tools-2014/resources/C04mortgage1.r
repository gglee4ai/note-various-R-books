## ----label=C04chunk12, echo=TRUE, size="scriptsize", cache=TRUE----------
mrate<-function(R){
   val<-0
   den<-1
   fact<-1/6
   term<-1
   rr<-R/200
   repeat { # main loop
      term<-term*fact*rr/den
      vallast<-val
      val<-val+term
#      cat("term =",term,"  val now ",val,"\n")
      if (val == vallast) break
      fact<-(fact - 1)
      den<-den+1
      if (den > 1000) stop("Too many terms in mrate")
   }
   val*100
}
A<-function(I,Rval){
   A<-(1+I/100)^6-(1+R/200)
}

Rvec<-c()
i.formula<-c()
i.root<-c()
i.mrate<-c()

for (r2 in 0:18){
   R<-r2/2 # rate per year
   Rvec<-c(Rvec, R)
   i.formula<-c(i.formula,100*((1+R/200)^(1/6)-1))
   i.root<-c(i.root, uniroot(A,c(0,20),tol=.Machine$double.eps,Rval=R)$root)
   i.mrate<-c(i.mrate,mrate(R))
}
   tabR<-data.frame(Rvec, i.mrate, (i.formula-i.mrate), (i.root-i.mrate))
   colnames(tabR)<-c("rate", "i.mrate", "form - mrate", "root - mrate")
   print(tabR)


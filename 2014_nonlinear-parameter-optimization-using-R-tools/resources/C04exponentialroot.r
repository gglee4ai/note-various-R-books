## ----label=C04chunk13,echo=TRUE,cache=TRUE-------------------------------
cat("exponential example\n")
require("rootoned")
alpha<-1.0
efn<-function(x, alpha) { exp(-alpha*x) - 0.2 }
zfn<-function(x) { x*0 }
tint<-c(0,100)
curve(efn(x,alpha=1.0), from=tint[1], to=tint[2], lty=2, ylab="efn(x, alpha)")
title(sub="dashed for alpha=1, dotted for alpha=0.2")
curve(zfn, add=TRUE)
curve(efn(x,alpha=0.02), add=TRUE, lty=3)
rform1<- -log(0.2)/1.0
rform2<- -log(0.2)/0.02
resr1<-uniroot(efn,tint,tol=1e-10, alpha=1)
cat("alpha = ",1.0,"\n")
cat("root(formula)=",rform1,"  root1d:",resr1$root," tol=",resr1$estim.prec,"  fval=",
         resr1$f.root,"  in   ",resr1$iter,"\n\n") 
resr2<-uniroot(efn,tint,tol=1e-10, alpha=0.02)
cat("alpha = ",0.02,"\n")
cat("root(formula)=",rform2,"  root1d:",resr2$root," tol=",resr2$estim.prec,"  fval=",
         resr2$f.root,"  in   ",resr2$iter,"\n\n") 
cat("\n")
cat("Now look for the root in [0,1]\n")
tint2a <- c(0, 1)
resr2a<-uniroot(efn,tint2a,tol=1e-10, alpha=0.02)

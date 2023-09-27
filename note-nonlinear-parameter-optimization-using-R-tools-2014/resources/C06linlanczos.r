# C06linlanc.R
source("C06lancdata.R", echo=FALSE) # to allow script to be self-standing

bb1s<-bb1[1:3] # shorten start, as only 3 starting nonlinear parameters
bb2s<-bb2[1:3]
llexp<-"y~cbind(exp(-a1*x), exp(-a2*x), exp(-a3*x))"
tnls1p<-try(nls(llexp, start=bb1s, data=Lanczos1, algorithm='plinear', 
    trace=TRUE, control=list(maxiter=20)))
strwrap(tnls1p)
tnls2p<-try(nls(llexp, start=bb2s, data=Lanczos1,  algorithm='plinear',
    trace=TRUE, control=list(maxiter=20)))
strwrap(tnls2p)


## ----label=C06lanc5, echo=TRUE, cache=TRUE-------------------------------
lanclin.res<-function (b, data) 
{ # restructured to allow for easier linearization
    xx <- data$x
    yy <- data$y
    res <- rep(NA, length(xx))
    m<-length(xx)
    n<-3
    A <- matrix(NA, nrow=m, ncol=n)
    for (j in 1:n) {
        A[,j]<-exp(-b[j]*xx)
    }
    lmod<-lsfit(A, yy, intercept=FALSE)
    res<-lmod$residuals
    attr(res, "coef")<-lmod$coef    
    res
}
bb1s
res1L<-lanclin.res(bb1s,data=Lanczos1)
cat("sumsquares via lanclin for bb1s start:",
      as.numeric(crossprod(res1L)),"\n")
tnlfbL1<-try(nlfb(bb1s, lanclin.res, trace=FALSE, data=Lanczos1))
tnlfbL1
## Get the linear coefficients
resL1<-lanclin.res(coef(tnlfbL1),data=Lanczos1)
resL1
## Now from bb2s
bb2s
res2L<-lanclin.res(bb2s,data=Lanczos1)
cat("sumsquares via lanclin for bb2s start:",
      as.numeric(crossprod(res2L)),"\n")
tnlfbL2<-try(nlfb(bb2s, lanclin.res, trace=FALSE, data=Lanczos1))
tnlfbL2
## Get the linear coefficients
resL2<-lanclin.res(coef(tnlfbL2),data=Lanczos1)
resL2

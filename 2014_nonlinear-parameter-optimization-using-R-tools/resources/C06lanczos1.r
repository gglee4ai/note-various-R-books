#  C06lanczos1.R

source("C06lancdata.R", echo=FALSE) # get the data and parameters

## ----label=C06lanc3, echo=TRUE, cache=TRUE-------------------------------
tnls1<-try(nls(lancexp, start=bb1, data=Lanczos1, trace=FALSE))
strwrap(tnls1)
tnls2<-try(nls(lancexp, start=bb2, data=Lanczos1, trace=FALSE))
strwrap(tnls2)
## nlmrt
require(nlmrt)
tnlxb1<-try(nlxb(lancexp, start=bb1, data=Lanczos1, trace=FALSE))
tnlxb1
tnlxb2<-try(nlxb(lancexp, start=bb2, data=Lanczos1, trace=FALSE))
tnlxb2
tnlfb1<-try(nlfb(bb1, lanc.res, trace=FALSE, data=Lanczos1))
tnlfb1
tnlfb2<-try(nlfb(bb2, lanc.res, trace=FALSE, data=Lanczos1))
tnlfb2
tnlfb1j<-try(nlfb(bb1, lanc.res, lanc.jac, trace=FALSE, data=Lanczos1))
tnlfb1j
tnlfb2j<-try(nlfb(bb2, lanc.res, lanc.jac, trace=FALSE, data=Lanczos1))
tnlfb2j

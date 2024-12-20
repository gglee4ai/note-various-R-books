## ----label=C07nleq-set1, echo=FALSE, cache=TRUE--------------------------

source("C07nleqslvx1.R", echo=FALSE) # to allow script to be self-standing

sumtbl<-matrix(NA, nrow=10, ncol=11)
# methods BBsolve, sane, dfsane, nleqslv(Newton), nleqslv(Broyden), nlfb, nls.lm, nlfbn, nls.lmn
irow<-0 # table row


## ----label=C07nleqset2, include=TRUE, cache=TRUE-------------------------
require(microbenchmark) # for timing
require(BB)
require(nleqslv)
xstart <- c(2,0.5)
fstart <- dslnex(xstart)
## xstart:
print(xstart)
## resids at start:
print(fstart)
# a solution is c(1,1)
print(dslnex(c(1,1)))


## ----label=C07nleqexBB, include=TRUE, cache=TRUE-------------------------
tabbx<-microbenchmark(abbx<-BBsolve(xstart, dslnex, quiet=TRUE))$time
cat("solution at ",abbx$par[1]," ",abbx$par[2],"\n")


## ----label=C07saveabbx, echo=FALSE, cache=TRUE---------------------------
irow<-irow+1
sumtbl[irow,1]<-"BBsolve    "
sumtbl[irow,2]<-abbx$par[1]
sumtbl[irow,3]<-abbx$par[2]
# functions
rr<-dslnex(abbx$par)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]

sumtbl[irow,6]<-abbx$feval # functions
sumtbl[irow,7]<-NA # jacs
sumtbl[irow,8]<-abbx$iter # iterations
sumtbl[irow,9]<-abbx$conv # codes

sumtbl[irow,10]<-mean(tabbx)
sumtbl[irow,11]<-sd(tabbx)


## ----label=C07nleqexsane, include=TRUE, cache=TRUE-----------------------
cl <- list(trace=FALSE)
# sane
tasx<-microbenchmark(asx <- sane(xstart, dslnex, quiet=TRUE, control=cl))$time
cat("solution at ",asx$par[1]," ",asx$par[2],"\n")


## ----label=C07saveasx, echo=FALSE, cache=TRUE----------------------------
irow<-irow+1
sumtbl[irow,1]<-"sane       "
sumtbl[irow,2]<-asx$par[1]
sumtbl[irow,3]<-asx$par[2]
rr<-dslnex(asx$par)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-asx$feval # functions
sumtbl[irow,7]<-NA # jacs
sumtbl[irow,8]<-asx$iter # iterations
sumtbl[irow,9]<-asx$convergence # codes

sumtbl[irow,10]<-mean(tasx)
sumtbl[irow,11]<-sd(tasx)


## ----label=C07nleqexdfsane, include=TRUE, cache=TRUE---------------------
tadx<-microbenchmark(adx <- dfsane(xstart, dslnex, quiet=TRUE, control=cl))$time
cat("solution at ",adx$par[1]," ",adx$par[2],"\n")


## ----label=C07saveadx, echo=FALSE, cache=TRUE----------------------------
irow<-irow+1

sumtbl[irow,1]<-"dfsane     "
sumtbl[irow,2]<-adx$par[1]
sumtbl[irow,3]<-adx$par[2]
rr<-dslnex(adx$par)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-adx$feval # functions
sumtbl[irow,7]<-NA # jacs
sumtbl[irow,8]<-adx$iter # iterations
sumtbl[irow,9]<-adx$convergence # codes

sumtbl[irow,10]<-mean(tadx)
sumtbl[irow,11]<-sd(tadx)


## ----label=C07exnleqslv, include=TRUE, cache=TRUE------------------------
require(nleqslv)
tanlx<-microbenchmark(anlx<-nleqslv(xstart, dslnex))$time
cat("solution at ",anlx$x[1]," ",anlx$x[2],"\n")


## ----label=C07saveanlx, echo=FALSE, cache=TRUE---------------------------
irow<-irow+1
sumtbl[irow,1]<-"nleqslv    "
sumtbl[irow,2]<-anlx$x[1]
sumtbl[irow,3]<-anlx$x[2]
rr<-dslnex(anlx$x)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-anlx$nfcnt # functions
sumtbl[irow,7]<-anlx$njcnt # jacs
sumtbl[irow,8]<-anlx$iter # iterations
sumtbl[irow,9]<-anlx$termcd # codes termcd should be 1?d
sumtbl[irow,10]<-mean(tanlx)
sumtbl[irow,11]<-sd(tanlx)


## ----label=C07exnleqslvN, include=TRUE, cache=TRUE-----------------------
tanlNx<-microbenchmark(anlNx<-nleqslv(xstart, dslnex, method="Newton"))$time
cat("solution at ",anlNx$x[1]," ",anlNx$x[2],"\n")


## ----label=C07saveanlNx, echo=FALSE, cache=TRUE--------------------------
irow<-irow+1
sumtbl[irow,1]<-"nleqslvN   "
sumtbl[irow,2]<-anlNx$x[1]
sumtbl[irow,3]<-anlNx$x[2]
rr<-dslnex(anlNx$x)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-anlNx$nfcnt # functions
sumtbl[irow,7]<-anlNx$njcnt # jacs
sumtbl[irow,8]<-anlNx$iter # iterations
sumtbl[irow,9]<-anlNx$termcd # codes termcd should be 1?d
sumtbl[irow,10]<-mean(tanlNx)
sumtbl[irow,11]<-sd(tanlNx)


## ----label=C07exnleqslvB, include=TRUE, cache=TRUE-----------------------
tanlBx<-microbenchmark(anlBx<-nleqslv(xstart, dslnex, method="Broyden"))$time
cat("solution at ",anlBx$x[1]," ",anlBx$x[2],"\n")


## ----label=C07saveanlBx, echo=FALSE, cache=TRUE--------------------------
irow<-irow+1
sumtbl[irow,1]<-"nleqslvB   "
sumtbl[irow,2]<-anlBx$x[1]
sumtbl[irow,3]<-anlBx$x[2]
rr<-dslnex(anlBx$x)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-anlBx$nfcnt # functions
sumtbl[irow,7]<-anlBx$njcnt # jacs
sumtbl[irow,8]<-anlBx$iter # iterations
sumtbl[irow,9]<-anlBx$termcd # codes termcd should be 1?d
sumtbl[irow,10]<-mean(tanlBx)
sumtbl[irow,11]<-sd(tanlBx)


## ----label=C07nlslm, include=TRUE, cache=TRUE----------------------------
require(minpack.lm)
takx<-microbenchmark(akx<-nls.lm(par=xstart, fn=dslnex, jac=jacdsln))$time
cat("solution at ",akx$par[1]," ",akx$par[2],"   with equation residuals \n")
print(dslnex(akx$par))


## ----label=C07saveakx, echo=FALSE, cache=TRUE----------------------------
irow<-irow+1
sumtbl[irow,1]<-"nls.lm     "
sumtbl[irow,2]<-akx$par[1]
sumtbl[irow,3]<-akx$par[2]
sumtbl[irow,4]<-akx$fvec[1]
sumtbl[irow,5]<-akx$fvec[2]
sumtbl[irow,6]<-NA # functions
sumtbl[irow,7]<-NA # jacs
sumtbl[irow,8]<-akx$niter # iterations
sumtbl[irow,9]<-NA # codes termcd should be 1?d
sumtbl[irow,10]<-mean(takx)
sumtbl[irow,11]<-sd(takx)


## ----label=C07nlslmnoJ, include=TRUE, cache=TRUE-------------------------
taknx<-microbenchmark(aknx<-nls.lm(par=xstart, fn=dslnex, jac=NULL))$time
cat("solution at ",aknx$par[1]," ",aknx$par[2],"   with equation residuals \n")
print(dslnex(aknx$par))


## ----label=C07saveaknx, echo=FALSE, cache=TRUE---------------------------
irow<-irow+1
sumtbl[irow,1]<-"nls.lm-noJ  "
sumtbl[irow,2]<-aknx$par[1]
sumtbl[irow,3]<-aknx$par[2]
sumtbl[irow,4]<-aknx$fvec[1]
sumtbl[irow,5]<-aknx$fvec[2]
sumtbl[irow,6]<-NA # functions
sumtbl[irow,7]<-NA # jacs
sumtbl[irow,8]<-aknx$niter # iterations
sumtbl[irow,9]<-NA # codes termcd should be 1?d
sumtbl[irow,10]<-mean(taknx)
sumtbl[irow,11]<-sd(taknx)


## ----label=C07nlfb, include=TRUE, cache=TRUE-----------------------------
require(nlmrt)
tajnx<-microbenchmark(ajnx<-nlfb(start=xstart, dslnex, jacdsln))$time
ajnxc<-coef(ajnx)
cat("solution at ",ajnxc[1]," ",ajnxc[2],
          "   with equation residuals \n")
print(dslnex(ajnx$coefficients))


## ----label=C07saveajnx, echo=FALSE, cache=TRUE---------------------------
irow<-irow+1
sumtbl[irow,1]<-"nlfb       "
sumtbl[irow,2]<-ajnx$coefficients[1]
sumtbl[irow,3]<-ajnx$coefficients[2]
rr<-dslnex(ajnx$coefficients)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-ajnx$feval # functions
sumtbl[irow,7]<-ajnx$jeval # jacs
sumtbl[irow,8]<-NA # iterations
sumtbl[irow,9]<-NA # codes termcd should be 1?d
sumtbl[irow,10]<-mean(tajnx)
sumtbl[irow,11]<-sd(tajnx)


## ----label=C07nlfbnoJ, include=TRUE, cache=TRUE--------------------------
tajnnx<-microbenchmark(ajnnx<-nlfb(start=xstart, dslnex, jac=NULL))$time
ajnnxc<-coef(ajnnx)
cat("solution at ",ajnnxc[1]," ",ajnnxc[2],"   with equation residuals \n")
print(dslnex(ajnnx$coefficients))


## ----label=C07saveajnnx, echo=FALSE, cache=TRUE--------------------------
irow<-irow+1
sumtbl[irow,1]<-"nlfb-noJ   "
sumtbl[irow,2]<-ajnnx$coefficients[1]
sumtbl[irow,3]<-ajnnx$coefficients[2]
rr<-dslnex(ajnnx$coefficients)
sumtbl[irow,4]<-rr[1]
sumtbl[irow,5]<-rr[2]
sumtbl[irow,6]<-ajnnx$feval # functions
sumtbl[irow,7]<-ajnnx$jeval # jacs
sumtbl[irow,8]<-NA # iterations
sumtbl[irow,9]<-NA # codes termcd should be 1?d
sumtbl[irow,10]<-mean(tajnnx)
sumtbl[irow,11]<-sd(tajnnx)


## ----label=C07optimx, include=TRUE---------------------------------------
require(optimx)
aox<-suppressWarnings(optimx(xstart, ssdsln, gr="grcentral", method="all"))
summary(aox, order=value)


## ----label=C07morenlslm, include=TRUE, cache=TRUE------------------------
JJ<-jacdsln(akx$par)
svd(JJ)$d
res<-dslnex(akx$par)
res
g<-t(JJ)%*%res
g
fn<-ssdsln(akx$par)
fn
HH<-hessian(ssdsln,akx$par)
HH
eigen(HH)$values


## ----label=C07nleqperf1, echo=FALSE, cache=TRUE--------------------------
rnames<-sumtbl[,1]
tpar<-matrix(as.numeric(sumtbl[,2:5]), nrow=10, ncol=4)
tcount<-matrix(as.numeric(sumtbl[,6:9]), nrow=10, ncol=4)
colnames(tpar)<-c("par1", "par2", "res1", "res2")
rownames(tpar)<-rnames
tpar<-round(tpar, digits=7)
colnames(tcount)<-c("fevals", "jevals", "niter", "ccode")
rownames(tcount)<-rnames
tcount<-round(tcount, digits=4)
ttime<-matrix(as.numeric(sumtbl[,10:11]), nrow=10, ncol=2)
ttime<-round(ttime/1000, digits=0)
rownames(ttime)<-rnames
colnames(ttime)<-c("mean(t)", "sd(t)")


## ----label=C07nleqperf2, echo=FALSE, cache=TRUE--------------------------
print(tpar)


## ----label=C07nleqperf3, echo=FALSE, cache=TRUE--------------------------
print(ttime)


## ----label=C07nleqperf4, echo=FALSE, cache=TRUE--------------------------
print(tcount)

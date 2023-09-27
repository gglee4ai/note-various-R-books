## ----label=C18trunif1, echo=TRUE-----------------------------------------
nnum<-100000L
cat("Time to generate ", nnum,"  is ")
tt<-system.time(xx<-runif(nnum))[[3]]
cat(tt,"\n")


## ----label=C18trunif2, echo=TRUE-----------------------------------------
nnum<-100000L
require("rbenchmark")
cat("Time to generate ", nnum,"  is ")
trb<-rbenchmark::benchmark(runif(nnum))
# print(str(trb))
print(trb)


## ----label=C18trunif3, echo=TRUE-----------------------------------------
nnum<-100000L
require("microbenchmark")
cat("Time to generate ", nnum,"  is ")
tmb<-microbenchmark(xx<-runif(nnum))
print(str(tmb))
print(tmb)


## ----label=C18timbench, echo=TRUE, eval=FALSE----------------------------
## ### Compute benchmark for machine in R
## ###  J C Nash 2013
busyfnloop <- function(k){
   for (j in 1:k){
    x<-log(exp(sin(cos(j*1.11))))
   }
   x
 }
busyfnvec <- function(k){
    vk<-1:k
    x<-log(exp(sin(cos(1.1*vk))))[k]
 }
if (! file.exists("./tmpdata/timbench.dput") ) {
require(microbenchmark, quietly=TRUE)
 nlist<-c(1e3, 1e4, 5e4, 1e5, 2e5)
 mlistl<-rep(NA, length(nlist))
 slistl<-mlistl
 mlistv<-mlistl
 slistv<-mlistl
 for (kk in 1:length(nlist)){
   k<-nlist[kk]
   cat("number of loops =",k,"\n")
   cat("loop\n")
   tt<-microbenchmark( busyfnloop(k) )
   mlistl[kk]<-mean(tt$time)*1e-6
   slistl[kk]<-sd(tt$time)*1e-6
   cat("vec\n")
   tt<-microbenchmark( busyfnvec(k) )
   mlistv[kk]<-mean(tt$time)*1e-6
   slistv[kk]<-sd(tt$time)*1e-6
 }
##  build table
 cvl <- slistl/mlistl
 cvv <- slistv/mlistv
 l2v <- mlistl/mlistv
 restime<-data.frame(nlist, mlistl, slistl, cvl,  mlistv, slistv, cvv, l2v)
 names(restime)<-c(" n","mean loop", "sd loop", "cv loop", "mean vec", "sd vec", "cv vec", "loop/vec")
 ## Times were converted to milliseconds
 restime
 dput(restime, file="filenameforyourmachine.dput")
} else {

## ----label=C18timbenchx, echo=FALSE--------------------------------------
  restime<-dget("tmpdata/timbench.dput")
}
options(width=100) # to get all the columns on the display
print(restime)

## ----label=C18forwhiletime, echo=TRUE, eval=FALSE------------------------
## #  forwhiletime.R
require(microbenchmark)
require(compiler)
## 
tfor <- function(n){
     for (i in 1:n) {
        xx<-exp(sin(cos(as.double(i))))
     }
     xx
}
## 
twhile <- function(n){
     i<-0
     while (i<n) {
        i<-i+1
        xx<-exp(sin(cos(as.double(i))))
     }
     xx
}

if (! file.exists("./tmpdata/fwtime.dput")) {
  n<-10000
  ## 
  timfor<-microbenchmark(tfor(n))
  timwhile<-microbenchmark(twhile(n))
  tforc<-cmpfun(tfor)
  twhilec<-cmpfun(twhile)
  timforc<-microbenchmark(tforc(n))
  timwhilec<-microbenchmark(twhilec(n))
  looptimes<-data.frame(timfor$time, timforc$time, timwhile$time, timwhilec$time)
  dput(looptimes, file="./tmpdata/fwtime.dput") 
} else {
## ----label=C18fwtime1, echo=FALSE----------------------------------------
  looptimes<-dget("/home/john/nlpor/includes/C18fwtimeJ6.dput")
}
print(colMeans(looptimes))

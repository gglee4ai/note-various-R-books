
## ----label=C18tbu1, echo=TRUE, eval=FALSE--------------------------------
 require(microbenchmark)
 require(Rvmmin)
 require(Rcgmin)
 sqs<-function(x) {
    sum( seq_along(x)*(x - 0.5*seq_along(x))^2)
 }
 sqsg<-function(x) {
    ii<-seq_along(x)
    g<-2.*ii*(x - 0.5*ii)
 }

if (! file.exists("./tmpdata/mybdtime.dput")) {
 xstrt<-rep(pi,20)
 lb<-rep(-100,20)
 ub<-rep(100,20) # very loose bounds
 bdmsk<-rep(1,20) # free parameters
 tvmu<-microbenchmark(avmu<-Rvmminu(xstrt, sqs, sqsg))
 tcgu<-microbenchmark(acgu<-Rcgminu(xstrt, sqs, sqsg))
 tvmb<-microbenchmark(avmb<-Rvmminb(xstrt, sqs, sqsg, bdmsk=bdmsk, lower=lb, upper=ub))
 tcgb<-microbenchmark(acgb<-Rcgminb(xstrt, sqs, sqsg, bdmsk=bdmsk, lower=lb, upper=ub))
 svmu<-summary(tvmu$time)
 cnames<-names(svmu)
 svmu<-as.numeric(svmu)
 svmb<-as.numeric(summary(tvmb$time))
 scgu<-as.numeric(summary(tcgu$time))
 scgb<-as.numeric(summary(tcgb$time))
 names(svmu)<-cnames
 names(svmb)<-cnames
 names(scgu)<-cnames
 names(scgb)<-cnames
 mytab<-rbind(svmu, svmb, scgu, scgb)
 rownames(mytab)<-c("Rvmminu", "Rvmminb", "Rcgminu", "Rcgminb")
 dput(mytab, file="./tmpdata/mybdtime.dput")
} else {
## ----label=C18tbu1x, echo=FALSE------------------------------------------
 mytab<-dget("./tmpdata/mybdtime.dput")
}

ftable(mytab)

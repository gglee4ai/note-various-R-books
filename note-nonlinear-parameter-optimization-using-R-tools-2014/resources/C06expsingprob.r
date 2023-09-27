## ----label=C06expsing1, echo=TRUE, cache=TRUE----------------------------
x<-c(60,80,100,120)
y<-c(0.8,6.5,20.5,45.9)
mydata<-data.frame(x,y)
pnames<-c("a", "b", "d")
npar<-length(pnames)
st<-c(1,1,1)
names(st)<-pnames
rnls<-try(nls(y ~ exp(a + b*x)+d,start=st, data=mydata), silent=TRUE)
if (class(rnls) == "try-error") { 
   cat("nls() failed (singular gradient?)\n") 
} else { 
   summary(rnls) 
}
require(nlmrt)
rnlx0<-try(nlxb(y ~ exp(a + b*x)+d,start=st, data=mydata), silent=TRUE)
cat("Found sum of squares ",rnlx0$ssquares,"\n")
print(rnlx0$coefficients)
## Now turn off relative offset test
rnlx1<-try(nlxb(y ~ exp(a + b*x)+d,start=st, data=mydata, 
         control=list(rofftest=FALSE)), silent=TRUE)
cat("Found sum of squares ",rnlx1$ssquares,"\n")
coef(rnlx1)
## And the sumsquares size test
rnlx2<-try(nlxb(y ~ exp(a + b*x)+d,start=st, data=mydata, 
         control=list(rofftest=FALSE, smallsstest=FALSE)), silent=TRUE)
cat("Found sum of squares ",rnlx2$ssquares,"\n")
coef(rnlx2)


## ----label=C06expsing2, echo=FALSE, cache=TRUE---------------------------
require(nlmrt)
x<-c(60,80,100,120)
y<-c(0.8,6.5,20.5,45.9)
mydata<-data.frame(x,y)
pnames<-c("a", "b", "d")
npar<-length(pnames)
st<-c(1,1,1)
names(st)<-pnames
debug<-FALSE
xmin<-c(0,0,0)
xmax<-c(8,8,8)
set.seed(123456)
nrep <- 1000 # make shorter to test
# set up structure to record results
#  need start, failnls, parnls, ssnls, fnsnls, failnlxb, parnlxb, ssnlxb, fnsnlxb
tmp<-matrix(NA, nrow=nrep, ncol=3*npar+7)
outcome<-as.data.frame(tmp)
rm(tmp)
colnames(outcome)<-c(paste("st-",pnames[[1]],''),
	paste("st-",pnames[[2]],''),
	paste("st-",pnames[[3]],''),
        "failnls", 
        paste("nls-",pnames[[1]],''),
	paste("nls",pnames[[2]],''),
	paste("nls-",pnames[[3]],''), 
        "ssnls",
	"fnsnls",
        "failnlxb", 
        paste("nlxb-",pnames[[1]],''),
	paste("nlxb-",pnames[[2]],''), 
	paste("nlxb-",pnames[[3]],''), 
	"ssnlxb", 
        "fnsnlxb", 
        "jsnlxb")

for (i in 1:nrep){

if (debug) cat (i,"  ")

st<-runif(3)
names(st)<-pnames
if (debug) print(st)
rnls<-try(nls(y ~ exp(a + b*x)+d,start=st, data=mydata), silent=TRUE)
if (class(rnls) == "try-error") {
   failnls<-TRUE
   parnls<-rep(NA,length(st))
   ssnls<-NA
   fnsnls<-NA
} else {
   failnls<-FALSE
   ssnls<-deviance(rnls)
   if (ssnls > 0.6) failnls<-TRUE
   parnls<-coef(rnls)
   fnsnls<-rnls$convInfo$finIter
}
names(parnls)<-pnames
if (debug) {
  cat("nls():")
  print(rnls)
}
rnlxb<-try(nlxb(y ~ exp(a + b*x)+d,start=st, data=mydata, 
        control=list(rofftest=FALSE, smallsstest=FALSE)), silent=TRUE)
if (class(rnlxb) == "try-error") {
   failnxlb<-TRUE
   parnlxb<-rep(NA,length(st))
   ssnlxb<-NA
   fnsnlxb<-NA
   jsnlxb<-NA
} else {
   failnlxb<-FALSE
   ssnlxb<-rnlxb$ssquares
   if (ssnlxb > 0.6) failnlxb<-TRUE
   parnlxb<-rnlxb$coefficients
   fnsnlxb<-rnlxb$feval
   jsnlxb<-rnlxb$jeval
}
names(parnls)<-pnames
if (debug) {
  cat("nlxb():")
  print(rnlxb)
  tmp<-readline()
  cat("\n")
  }
 solrow<-c(st, failnls=failnls, parnls, ssnls=ssnls, fnsnls=fnsnls,
     failnlxb=failnlxb, parnlxb, ssnlxb=ssnlxb, fnsnlxb=fnsnlxb, 
     jsnlxb=jsnlxb)
  outcome[i,]<-solrow
} # end loop

cat("Proportion of nls  runs that failed = ",sum(outcome$failnls)/nrep,"\n")
cat("Proportion of nlxb runs that failed = ",sum(outcome$failnlxb)/nrep,"\n")
cat("Average iterations for nls =",mean(outcome$fnsnls, na.rm=TRUE),"\n")
cat("Average jacobians for nlxb =",mean(outcome$jsnlxb, na.rm=TRUE),"\n")
cat("Average residuals for nlxb =",mean(outcome$fnsnlxb, na.rm=TRUE),"\n")

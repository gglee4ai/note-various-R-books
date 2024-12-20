## ----label=C18upnew1, echo=TRUE------------------------------------------
require(microbenchmark)
# Different ways to do the BFGS update
bfgsout<-function(B, t, y, D1, D2){
    Bouter <- B - (outer(t, y) + outer(y, t) - D2 * outer(t, t))/D1
}

bfgsloop<-function(B, t, y, D1, D2){
   A<-B 
   n<-dim(A)[1]
   for (i in 1:n) {
       for (j in 1:n) {
           A[i,j]<-A[i,j]-(t[i]*y[j]+y[i]*t[j]-D2*t[i]*t[j])/D1
       }
   }
   A
}

bfgsmult<-function(B, t, y, D1, D2){
   A<-B 
   A<-A - (t%*%t(y) + y%*%t(t) - D2 * t%*%t(t))/D1
}

bfgsx<-function(B, t, y, D1, D2){ # fastest
   A<-outer(t,y) 
   A<- B - (A+t(A) - D2 * outer(t,t))/D1
}

# Recover saved data
c<-dget(file="testc.txt")
t<-dget(file="testt.txt")
B<-dget(file="testB.txt")
# ychk<-dget(file="testy.txt")
tD1sum<- microbenchmark(D1a <- sum(t * c))$time # faster?
tD1crossprod<-microbenchmark(D1c<-as.numeric(crossprod(t, c)))$time 
#summary(tD1sum)
#summary(tD1crossprod)
mean(tD1crossprod-tD1sum)
cat("abs(D1a-D1c)=",abs(D1a-D1c),"\n")
if (D1a <=0) stop("D1 <= 0")
tymmult<-microbenchmark(y <- as.vector(B %*% c))$time
tycrossprod<-microbenchmark(ya<- crossprod(B, c))$time # faster
#summary(tymmult)
#summary(tycrossprod)
mean(tymmult-tycrossprod)
cat("max(abs(y-ya)):", max(abs(y-ya)),"\n")
td2vmult<-microbenchmark(D2a <- as.double(1 + (t(c) %*% y)/D1a))$time
td2crossprod<-microbenchmark(D2b <- as.double(1+crossprod(c,y)/D1a))$time # faster
cat("abs(D2a-D2b)=",abs(D2a-D2b),"\n")
#summary(td2vmult)
#summary(td2crossprod)
mean(td2vmult-td2crossprod)
touter<-microbenchmark(Bouter <- bfgsout(B, t, y, D1a, D2a))$time
tbloop<-microbenchmark(Bloop<-bfgsloop(B, t, y, D1a, D2a))$time
tbmult<-microbenchmark(Bmult<-bfgsloop(B, t, y, D1a, D2a))$time
tboutsave<-microbenchmark(Boutsave<-bfgsx(B, t, y, D1a, D2a))$time # faster?
## summary(touter)
## summary(tbloop)
## summary(tbmult)
## summary(tboutsave)
dfBup<-data.frame(touter, tbloop, tbmult, tboutsave)
colnames(dfBup)<-c("outer", "loops", "matmult", "out+save")
boxplot(dfBup)
title("Comparing timings for BFGS update")
## check computations are equivalent
max(abs(Bloop-Bouter))
max(abs(Bmult-Bouter))
max(abs(Boutsave-Bouter))

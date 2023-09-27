## ----label=C14nlrq1, echo=TRUE, cache=TRUE-------------------------------
y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
       38.558, 50.156, 62.948, 75.995, 91.972)
t<-1:12
weeddata<-data.frame(y=y, t=t)
require(quantreg)
formulas<-y ~ 100*sb1/(1+10*sb2*exp(-0.1*sb3*t))
snames<-c("sb1", "sb2", "sb3")
st<-c(1,1,1)
names(st)<-snames
anlrqsh1 <- nlrq(formulas, start=st, data=weeddata)
summary(anlrqsh1)


## ----label=C14nlrq2, echo=TRUE, cache=TRUE-------------------------------
st<-c(2,5,3)
names(st)<-snames
anlrqsh2 <- nlrq(formulas, start=st, data=weeddata)
summary(anlrqsh2)

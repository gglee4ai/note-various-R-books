## ----label=C06nlschunk01, echo=TRUE--------------------------------------
options(width=60)
pastured <- data.frame(
time=c(9, 14, 21, 28, 42, 57, 63, 70, 79),
yield= c(8.93, 10.8, 18.59, 22.33, 39.35, 
         56.11, 61.73, 64.62, 67.08))
regmod <- "yield ~ t1 - t2*exp(-exp(t3+t4*log(time)))"
ones <- c(t3=1, t4=1, t1=1, t2=1) # all ones start
huetstart <- c(t3=0, t4=1, t1=70, t2=60)


## ----label=C06nlschunk04, echo=TRUE--------------------------------------
anls <- try(nls(regmod, start=ones, trace=FALSE, data=pastured))
print(strwrap(anls))
anlsx <- try(nls(regmod, start=huetstart, trace=FALSE, data=pastured))
print(strwrap(anlsx))

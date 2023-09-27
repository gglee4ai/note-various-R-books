## ----label=C06nlschunk05b, echo=TRUE-------------------------------------
require(nls2)
set.seed(123) # for reproducibility
regmodf<-as.formula(regmod) # just in case
m100<-c(t1=-100, t2=-100, t3=-100, t4=-100)
p100<- (-1)*m100
gstart<-data.frame(rbind(m100, p100))
anls2 <- try(nls2(regmodf, start=gstart, data=pastured, 
  algorithm="random-search", control=list(maxiter=1000)))
print(anls2)

## ----label=C06nlschunk05c, echo=TRUE-------------------------------------
require(nls2)
set.seed(123) # for reproducibility
plinform <- yield ~ cbind(1, - exp(-exp(t3+t4*log(time))))
gstartpl<-data.frame(rbind(c(-10, 1), c(10, 8)))
names(gstartpl)<-c("t3", "t4")
anls2plb <- try(nls2(plinform, start=gstartpl, data=pastured, 
  algorithm="plinear-brute", control=list(maxiter=200)))
print(anls2plb)
## ====================================
anls2plr <- try(nls2(plinform, start=gstartpl, data=pastured, 
    algorithm="plinear-random", control=list(maxiter=200)))
print(anls2plr)




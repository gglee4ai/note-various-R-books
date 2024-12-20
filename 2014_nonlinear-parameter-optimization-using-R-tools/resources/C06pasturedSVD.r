## ----label=C06nlschunk03, echo=TRUE, cache=TRUE--------------------------
pastjac<-model2jacfun(regmod, ones, funname="pastjac")
J1<-pastjac(ones, yield=pastured$yield, time=pastured$time)
svd(J1)$d
J2<-pastjac(huetstart, yield=pastured$yield, time=pastured$time)
svd(J2)$d


## ----label=C06nlschunk03a, echo=TRUE, cache=TRUE, fig.height=3.5---------
Jnmrtx<-pastjac(coef(anmrtx), yield=pastured$yield, time=pastured$time)
svals<-svd(Jnmrtx)$d
barplot(svals,main="Singular values at nlxb solution to pasture problem", horiz=TRUE)

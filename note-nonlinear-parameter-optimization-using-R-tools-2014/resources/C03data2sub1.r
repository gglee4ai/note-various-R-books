## ----ch03, child='nlpor-structure-and-interfaces.Rnw'--------------------
## ----label=C03data2sub1, echo=TRUE, cache=TRUE---------------------------
xx<-1:12
yy<-exp(-0.1*sin(0.3*xx))
require(minpack.lm, quietly=TRUE)
strt1 <- list(p1=0, p2=1)
## here we do not specify any exogenous data
anls1<-nlsLM(yy~exp(p1*sin(p2*xx)), start=strt1, trace=FALSE)
anls1
rss<-function(par){
    p1<-par[1]
    p2<-par[2]
    res<-exp(p1*sin(p2*xx))-yy
    sum(res*res)
}
## check the initial sum of squares
print(rss(c(-0.1, 0.3)))
## and the final sum of squares
print(rss(coef(anls1)))
# Now try with an optimizer (default method=Nelder-Mead)
anm<-optim(strt1, rss)
anm
## But (at time of writing) nlmrt requires explicit data
mydat<-data.frame(xx=xx, yy=yy)
require(nlmrt, quietly=TRUE)
anlxb1<-nlxb(yy~exp(p1*sin(p2*xx)), start=strt1, trace=FALSE, 
   data=mydat)
## Insert following into call to get a more aggressive search
##   control=list(roffset=FALSE, smallsstest=FALSE)
print(anlxb1)


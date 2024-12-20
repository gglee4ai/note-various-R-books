# C06tslruns.R

# re-create the data so script is self-standing
set.seed(1235)
x <- 1:40
xint<-20.5*rep(1,length(x))
sla <-  0.5
slb <- -0.5
yint<-30
idx <- which(x <= xint)
ymod <- { yint + (x-xint)*slb }
ymod[idx]<-yint+(x[idx] - xint[idx])*sla
ydata<-ymod+rnorm(length(x),0,2)
# end of data creation



tslexp <- "y ~ (yint + (x-xint)*slb)*(x >= xint) + (yint + (x-xint)*sla)*( x < xint)"
mydf<-data.frame(x=x, y=ydata)
mnls<-try(nls(tslexp, trace=FALSE, data=mydf))
strwrap(mnls)
mystart<-c(xint = 1, yint=1, sla=1, slb=1)
mnls<-try(nls(tslexp, start=mystart, trace=FALSE, data=mydf))
strwrap(mnls)
myst2<-c(xint = 15, yint=25, sla=1, slb=-1)
mnls2<-try(nls(tslexp, start=myst2, trace=FALSE, data=mydf))
summary(mnls2)
mnls2$m$deviance()


## ----label=C06tsl3, echo=TRUE, cache=TRUE--------------------------------
require(nlmrt)
mnlxb<-try(nlxb(tslexp, start=mystart, trace=FALSE, data=mydf))
strwrap(mnlxb)


## ----label=C06tsl4, echo=TRUE, cache=TRUE--------------------------------
myres<-model2resfun(tslexp, mystart)
## Now check the calculation
tres<-myres(myst2, x=mydf$x, y=mydf$y)
tres
## But jacobian fails
jactry<-try(myjac<-model2jacfun(tslexp, mystart))
strwrap(jactry)
## Try nlfb with no jacfn.
mnlfb<-try(nlfb(mystart, resfn=myres, trace=FALSE, x=mydf$x, y=mydf$y))
mnlfb
## Also try from better start
mnlfb2<-try(nlfb(myst2, resfn=myres, trace=FALSE, x=mydf$x, y=mydf$y))
mnlfb2
## It will also work with
## mnlfb2j<-try(nlfb(myst2, resfn=myres, jacfn=NULL, trace=FALSE, x=mydf$x, y=mydf$y))
## or with Package pracma's function lsqnonlin (but there is not a good summary())
require(pracma)
alsqn<-lsqnonlin(myres, x0=myst2, x=mydf$x, y=mydf$y)
print(alsqn$ssq)
print(alsqn$x)

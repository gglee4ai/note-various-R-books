## ----label=C06tsl1, echo=TRUE, cache=TRUE, fig.height=4------------------
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
plot(x, ymod, type='l', ylim=c(0,40), ylab="y")
points(x, ydata)
title(main="2 straight lines data")
title(sub="Lines are those used to generate data")

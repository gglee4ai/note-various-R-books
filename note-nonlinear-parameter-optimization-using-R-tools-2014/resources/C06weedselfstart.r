## ----label=C06hobbselfstart2, echo=TRUE----------------------------------
## Put in weeddata here as precaution. Maybe reset workspace.
anlss2<-nls(y~SSlogis(t, p1, p2, p3), data=weeddata)
summary(anlss2)
anlss2$m$deviance()

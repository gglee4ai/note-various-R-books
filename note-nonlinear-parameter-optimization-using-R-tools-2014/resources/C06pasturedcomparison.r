# C06pasturedcomparison.R

## Note that we need to run the other scripts first.

source("C06pastured1.R", echo=FALSE)
source("C06pasturednlxb.R", echo=FALSE)
source("C06pasturedSVD.R", echo=FALSE)
X11() # ensure new graphics screen
source("C06pasturednlsLM.R", echo=FALSE)
source("C06pasturednls2.R", echo=FALSE)


## ----label=C06nlschunk05d, echo=TRUE, fig.height=4.75--------------------
require(nlmrt)
splb<-coef(anls2plb); splr<-coef(anls2plr)
names(splb)<-names(huetstart); names(splr)<-names(huetstart)
anlsfromplb<-nls(regmod, start=splb, trace=FALSE, data=pastured)
anlsfromplr<-nls(regmod, start=splr, trace=FALSE, data=pastured)
agomp<-wrapnls(regmodf, start=huetstart, data=pastured)
fitnlxb<-fitted(agomp); fitnls2b<-fitted(anls2plb); fitnls2r<-fitted(anls2plr)
plot(pastured$time, fitnlxb, type='l', lwd=2, xlab="time")
points(pastured$time, fitnls2b, col='red', type='l', lty="dashed", lwd=2)
points(pastured$time, fitnls2r, col='blue', type='l', lty="dotdash", lwd=2)
points(pastured$time, pastured$yield)
title(main="Fitted models for pasture data")
sstr<-"best = solid line, nls2.plinear.brute = dashed, nls2.plinear.random = dotdash"
title(sub=sstr, cex.sub=0.8)
resnlxb<-pastured$yield - fitnlxb; resnls2b<-pastured$yield - fitnls2b
resnls2r<-pastured$yield - fitnls2r


## ----label=C06nlschunk05e, echo=FALSE, cache=TRUE, fig.height=4.75-------
X11() # added to website file to avoid overwrite of screen plot
boxplot(data.frame(resnlxb, resnls2b, resnls2r))
title(main="Model residuals")
title(sub="best, nls2.plinear.brute, nls2.plinear.random")

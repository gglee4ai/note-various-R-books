## ----label=C05findwobl1a, echo=TRUE, cache=FALSE, fig.height=5-----------
woblramp<-function(x){ (-3+0.1*x)*cos(x*2) }
require(pracma, quietly=TRUE)
wmins<-findmins(woblramp, a=-10, b=10)
curve(woblramp, from=-10, to=10)
ymins<-woblramp(wmins) # compute the function at minima
points(wmins, ymins, col='red') # and overprint on graph
title(main="WOBLRAMP function with minima graphed")

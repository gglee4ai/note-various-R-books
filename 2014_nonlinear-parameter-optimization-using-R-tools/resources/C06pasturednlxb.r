## ----label=C06nlschunk02, echo=TRUE, cache=TRUE--------------------------
require(nlmrt)
anmrt <- nlxb(regmod, start=ones, trace=FALSE, data=pastured)
anmrt
anmrtx <- try(nlxb(regmod, start=huetstart, trace=FALSE, data=pastured))
anmrtx

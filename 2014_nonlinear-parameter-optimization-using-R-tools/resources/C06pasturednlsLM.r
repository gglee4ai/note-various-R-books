## ----label=C06nlsLM1, echo=TRUE------------------------------------------
require(minpack.lm)
aminp <- try(nlsLM(regmod, start=ones, trace=FALSE, data=pastured))
summary(aminp)
aminpx <- try(nlsLM(regmod, start=huetstart, trace=FALSE, data=pastured))
print(aminpx)

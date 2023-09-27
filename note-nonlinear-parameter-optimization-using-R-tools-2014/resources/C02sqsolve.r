## ----label=C02sqsolve, echo=TRUE, cache=TRUE-----------------------------
x <- c(0.1, 0.8)
source("C02sqfn.R")
source("C02steepdesc.R")
asqsd <- stdesc(x, sq.f, sq.g, control=list(trace=1))
print(asqsd)

## ----label=C18rprof1, echo=TRUE------------------------------------------
system("Rscript --vanilla doprof.R")
myprof<-summaryRprof("testjohn1.txt")
myprof$by.self

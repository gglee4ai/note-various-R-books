## ----label=C11ljexp, cache=TRUE, echo=TRUE-------------------------------
# The L-J potential function
 ljfne<-function(logr, ep=.6501696, sig=.3165555){
    r<-exp(logr)
    fn<-4*ep*((sig/r)^12 - (sig/r)^6)
 }
# Use next line to see a graph of the transformed function
# curve(ljfne, from=log(0.3), to=log(1))
min<-optimize(ljfne, interval=c(-10, 10))
print(min)
cat("In the original scale, minimum is at ", exp(min$minimum), "\n")

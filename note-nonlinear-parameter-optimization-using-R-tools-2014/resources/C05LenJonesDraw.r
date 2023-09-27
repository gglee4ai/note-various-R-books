## ----label=C05lj1, echo=TRUE, cache=TRUE---------------------------------
# The L-J potential function
 ljfn<-function(r, ep=.6501696, sig=.3165555){
    fn<-4*ep*((sig/r)^12 - (sig/r)^6)
 }
min<-optimize(ljfn, interval=c(0.001, 5))
print(min)
## Now graph it
curve(ljfn, from=0.3, to=1)
points(min$minimum, min$objective, pch=19)
title(main="Lennard-Jones minimum")
title(sub="Minimum found is marked with a dot")

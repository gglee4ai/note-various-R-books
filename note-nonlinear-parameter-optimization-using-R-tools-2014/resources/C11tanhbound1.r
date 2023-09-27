## ----label=C11tanhbound1, echo=FALSE, cache=TRUE, fig.height=3, fig.width=5----
lower<--4
upper<-10
par(cex=0.6) # scale plotting symbol
x<-lower+(upper-lower)*(0:100)/100
plot(x, lower + (upper - lower)/2 * (1 + tanh(x)), type='l')
points(x, lower + (upper - lower)/2 * (1 + tanh(0.25*x)), cex=0.05)

# C10derivtab.R

genrose.f <-  function(x, gs=100) { 
# objective function 
# A generalization of the Rosenbrock banana valley function to n parameters 
n  <-  length(x) 
fval <- 1.0 + sum(gs*(x[1:(n-1)]^2 - x[2:n])^2 + (x[2:n] - 1)^2) 
return(fval) 
}

genrose.g <- function(x, gs=100){ 
# vectorized gradient for genrose.f 
n <- length(x) 
gg <- as.vector(rep(0, n)) 
tn <- 2:n 
tn1 <- tn - 1 
z1 <- x[tn] - x[tn1]^2 
z2 <- 1 - x[tn] 
gg[tn] <- 2 * (gs * z1 - z2) 
gg[tn1] <- gg[tn1] - 4 * gs * x[tn1] * z1 
return(gg) 
}

if ("package:pracma" %in% search()) {
detach(package:pracma) # ensure right grad() fn
} 

fwgapp <- function(fn, xx, epstol=1.e-07,...) { 
# Simple forward approx using 1E-7 tol (scaled)
##	epstol <- 1.0E-7
	f0 <- fn(xx, ...)
	ll <- length(xx)
	gg <- rep(NA,ll)
	for (ii in 1:ll) {
		xt <- xx
		delta <- epstol * (abs(xt[ii]) + epstol)
		xt[ii] <- xx[ii] + delta
		ft <- fn(xt, ...)
		gg[ii] <- (ft - f0) / delta
	}
	gg
}

centgapp <- function(fn, xx, epstol=1.e-07,...) { 
# Simple central approx using 1E-7 tol (scaled)
##	epstol <- 1.0E-7
	f0 <- fn(xx, ...)
	ll <- length(xx)
	gg <- rep(NA,ll)
	for (ii in 1:ll) {
		xt <- xx
		delta <- epstol * (abs(xt[ii]) + epstol)
		xt[ii] <- xx[ii] + delta
		ff <- fn(xt, ...)
		xt[ii] <- xx[ii] - delta
                fb <- fn(xt, ...)
		gg[ii] <- 0.5*(ff - fb) / delta
	}
	gg
}

##################

vnorm<-function(x){sqrt(mean(x^2))}
dummy<-function(x) {y<-1}

# run the tests

require(numDeriv)

maxn<-10
n<-rep(NA,maxn)
ag<-rep(NA,maxn)
atym<-rep(NA,maxn)
ntym<-rep(NA,maxn)
ftym<-rep(NA,maxn)
xtym<-rep(NA,maxn)
ctym<-rep(NA,maxn)
nrat<-rep(NA,maxn)
frat<-rep(NA,maxn)
xrat<-rep(NA,maxn)
crat<-rep(NA,maxn)
nabs<-rep(NA,maxn)
fabs<-rep(NA,maxn)
cabs<-rep(NA,maxn)
xabs<-rep(NA,maxn)
nrel<-rep(NA,maxn)
frel<-rep(NA,maxn)
crel<-rep(NA,maxn)
xrel<-rep(NA,maxn)

mult<-2
trep=1000L

for (ni in 1:maxn) {
	nn<-mult*ni # number of elements
	xx<-rep(pi/2,nn) # Choose pi/2 to avoid integer numbers
	## cat(nn, "\n")
        ga<-genrose.g(xx,gs=100.0)
        gn<-grad(genrose.f,xx,gs=100.0)
        gf<-fwgapp(genrose.f,xx,gs=100.0)
        gc<-centgapp(genrose.f,xx,gs=100.0)
        gx<-grad(genrose.f,xx,method="complex",gs=100.0)
	ntime2<-system.time(for (i in 1:trep) {gn<-grad(genrose.f,xx)})[3]
	ctime2<-system.time(for (i in 1:trep) {gc<-centgapp(genrose.f,xx)})[3]
	xtime2<-system.time(for (i in 1:trep) {gx<-grad(genrose.f,xx,method="complex")})[3]
	ftime2<-system.time(for (i in 1:trep) {gf<-fwgapp(genrose.f,xx)})[3]
        atime2<-system.time(for (i in 1:trep) {ga<-genrose.g(xx)})[3]
	tt<-c(ta=atime2, tn=ntime2, tf=ftime2, tx=xtime2, tc=ctime2)
	# print(tt)
	n[ni]<-nn
	atym[ni]<-atime2
	ntym[ni]<-ntime2
	ftym[ni]<-ftime2
	ctym[ni]<-ctime2
	xtym[ni]<-xtime2

	ag[ni]<-vnorm(ga)
	nrat[ni]<-ntime2/atime2
	frat[ni]<-ftime2/atime2
	crat[ni]<-ctime2/atime2
	xrat[ni]<-xtime2/atime2

	nabs[ni]<-max(abs(gn-ga))
	fabs[ni]<-max(abs(gf-ga))
	cabs[ni]<-max(abs(gc-ga))
	xabs[ni]<-max(abs(gx-ga))
	
	nrel[ni]<-nabs[ni]/max(abs(ga))
	frel[ni]<-fabs[ni]/max(abs(ga))
	crel[ni]<-cabs[ni]/max(abs(ga))
	xrel[ni]<-xabs[ni]/max(abs(ga))
    ##    tmp<-readline("next")

}
derans<-data.frame(n,atym,ntym,ftym,ctym,xtym,
    nrat,frat,crat,xrat,nabs,ag,fabs,cabs,xabs,nrel,frel,crel,xrel)


## ----label=C10derivtab3, cache=TRUE, echo=FALSE, fig.height=6.5----------
dtime<-data.frame(derans$n, derans$atym, derans$ntym, derans$ftym, derans$ctym, derans$xtym)
leg.txt <- c("analytic","numDeriv","forward","central","complexstep") 
names(dtime)<-c("np",leg.txt)
attach(dtime)
plot(np, numDeriv, xlab="npar", ylab="time", pch=1, ylim=c(0,7.5))
points(np, complexstep, pch=3)
points(np, analytic, pch=20)
points(np, forward, pch=4)
points(np, central, pch=2)
title(main="Seconds for 1000 executions of approximations")
title(sub="Genrose function")
y.leg <- c(5.5, 5, 4.5, 4, 3.5)
for (i in 1:5)
  legend(1.2, y.leg[i], leg.txt[i], pch = c(20,1,4,2,3)[i], box.lwd=NA)
detach(dtime)


## ----label=C10derivtab4, cache=TRUE, echo=FALSE--------------------------
drelerr<-data.frame(derans$n, derans$frel, derans$crel, derans$nrel, derans$xrel)
leg.txt<-c("forward", "central","numDeriv", "complexstep")
names(drelerr)<-c("npar",leg.txt)
require(pander)
pandoc.table(signif(drelerr,3))

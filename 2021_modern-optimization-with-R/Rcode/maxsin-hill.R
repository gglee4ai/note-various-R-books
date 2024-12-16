### maxsin-hill.R file ###

# load the hill climbing methods
source("hill.R") 
source("hill2.R")

intbin=function(x) # convert binary to integer
{ sum(2^(which(rev(x==1))-1)) } # explained in Chapter 3
# max sin of binary raw object x (evaluation function):
maxsin=function(x,Dim) sin(pi*(intbin(x))/(2^Dim))

# hill climbing variants for max sin:
D=8 # dimension
# initial solution:
s=rep(0,D) # c(0,0,0,0,...) 
C=list(maxit=10,REPORT=1) # maximum of 10 iterations
lower=rep(0,D); upper=rep(1,D)
ichange=function(par,lower,upper) # integer change
{ hchange(par,lower,upper,rnorm,mean=0,sd=1) }

set.seed(123) # set for replicability

# steepest ascent with 5 searches in each iteration:
C1=C;C1$N=5 
cat("steepest ascent: N=",C1$N,"\n")
sa_hclimbing(s,maxsin,change=ichange,lower=lower,upper=upper,
            control=C1,type="max",Dim=D) # Dim used by maxsin

# low probability schochastic hill climbing:
C2=C;C2$P=0.2 
cat("stochastic hill climbing: P=",C2$P,"\n")
st_hclimbing(s,maxsin,change=ichange,lower=lower,upper=upper,
           control=C2,type="max",Dim=D) # Dim used by maxsin

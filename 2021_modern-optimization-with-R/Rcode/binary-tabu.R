### binary-tabu.R file ###
library(tabuSearch) # load tabuSearch package

# tabu search for sum of bits:
sumbin=function(x) (sum(x)) # sum of bits
D=8 # dimension
s0=rep(0,D) # c(0,0,0,0,...)

cat("sum of bits (D=",D,")\n",sep="")
s1=tabuSearch(D,iters=2,objFunc=sumbin,config=s0,neigh=2,
             listSize=4,nRestarts=1)
b=which.max(s1$eUtilityKeep) # best index
cat("best:",s1$configKeep[b,],"f:",s1$eUtilityKeep[b],"\n")

# tabu search for max sin:
intbin=function(x) sum(2^(which(rev(x==1))-1))
maxsin=function(x) # max sin (explained in Chapter 3)
{ D=length(x);x=intbin(x); return(sin(pi*(as.numeric(x))/(2^D))) }
D=8
cat("max sin (D=",D,")\n",sep="")
s2=tabuSearch(D,iters=2,objFunc=maxsin,config=s0,neigh=2,
             listSize=4,nRestarts=1)
b=which.max(s2$eUtilityKeep) # best index
cat("best:",s2$configKeep[b,],"f:",s2$eUtilityKeep[b],"\n")

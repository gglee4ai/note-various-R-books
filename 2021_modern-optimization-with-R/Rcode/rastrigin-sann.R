### rastrigin-sann.R file ###

# execute 3 simulation annealing optimizations for rastrigin D=30

library(NMOF) # load NMOF
library(GenSA) # load GenSA

source("hill.R") # get hchange

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
D=30 

set.seed(123) # set for replicability

# common setup for the 3 executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
par=runif(D,lower[1],upper[1]) # initial search point
maxit=10000 # maximum number of iterations
temp=1000 # initial temperature
repit=2000 # report every repit iterations

# control argument for optim:
C1=list(maxit=maxit,temp=temp,trace=TRUE,REPORT=repit/10)
# change function (gr argument):
rchange=function(par) # real change
{ D=length(par)
  # upper and lower are defined globally
  hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=1)
}
cat("SANN optim execution:\n") 
s1=optim(par,rastrigin,gr=rchange,method="SANN",control=C1)
cat("best:",s1$par,"f:",s1$value,"\n")

# algo configuration: 
algo=list(nI=maxit,initT=temp,x0=par,neighbour=rchange,printDetail=repit,storeF=TRUE)
cat("SAopt optim execution:\n") 
s2=SAopt(rastrigin,algo)
cat("best:",s2$xbest,"f:",s2$OFvalue,"\n")

C3=list(maxit=maxit,temperature=temp,verbose=TRUE)
cat("GenSA optim execution:\n") 
s3=GenSA(par,rastrigin,lower=lower,upper=upper,control=C3)
cat("best:",s3$par,"f:",s3$value,"\n")

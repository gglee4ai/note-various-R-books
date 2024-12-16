library(irace)
library(tabuSearch) # load tabuSearch package
source("functions.R") # load the profit function

# tabu search for bag prices:
D=5 # dimension (number of prices)
MaxPrice=1000
Dim=ceiling(log(MaxPrice,2)) # size of each price (=10)
size=D*Dim # total number of bits (=50)
s0=sample(0:1,size,replace=TRUE) # initial search

intbin=function(x) # convert binary to integer
{ sum(2^(which(rev(x==1))-1)) } # explained in Chapter 3

bintbin=function(x) # convert binary to D prices
{ # note: D and Dim need to be set outside this function
  s=vector(length=D) 
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*Dim+1;end=ini+Dim-1
    s[i]=intbin(x[ini:end])
  }
  return(s)
}

bprofit=function(x) # profit for binary x
{ s=bintbin(x) 
  if(sum(s>MaxPrice)>0) f=-Inf # death penalty
  else f=profit(s)
  return(f)
}

# irace parameters:
#  neigh from 1 to size
#  listSize from 2 (minimum) to twice the default (2*9=18)
parameters.txt='
neigh "" i (1, 50)
listSize "" i (2, 18)
'
parameters=readParameters(text=parameters.txt)

cat("initial:",bintbin(s0),"f:",bprofit(s0),"\n")

# evaluate each irace configuration:
runner=function(experiment,scenario)
{
 C=experiment$configuration # get current irace configuration
 s=tabuSearch(size,iters=100,objFunc=bprofit,config=s0,nRestarts=1, # fixed part
              neigh=C$neigh,listSize=C$listSize) # irace part
 b=which.max(s$eUtilityKeep) # best index
 # since tabuSearch performs maximization, return -profit:
 return(list(cost=-1*s$eUtilityKeep[b])) 
}

scenario=list(targetRunner=runner,
              instances=1, # not used but needs to be defined
              maxExperiments=100, # 100 calls to targetRunner  
              logFile = "") # do not create log file

ir=irace(scenario=scenario,parameters=parameters)
configurations.print(ir)

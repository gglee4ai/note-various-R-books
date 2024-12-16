### rastrigin-irace.R file ###

# tune SAopt parameter for rastrigin D=30

library(NMOF) # load NMOF
library(irace) # load irace
source("hill.R") # get hchange

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
D=30 

set.seed(123) # set for replicability

# setup rastrigin experiments:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
par=runif(D,lower[1],upper[1]) # fixed initial search point

rchange=function(par) # real change
{ D=length(par)
  # upper and lower are defined globally
  hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=1)
}

# irace elements: parameters and scenario --------------------
# 4 parameters for irace: i - integer, r - real value
parameters.txt='
nT "" i (1, 20)
nD "" i (1, 4000)
initT "" r (0.1, 1000.0)
alpha "" r (0.0, 1.0)
'
parameters=readParameters(text=parameters.txt)

# targetRunner function: call SAopt for an irace configuration
rastrigin_run=function(experiment,scenario)
{
  C=experiment$configuration # get current irace configuration
  # set algo with the irace configuration values
  algo=list(x0=par,nI=100,neighbour=rchange,    # fixed part
            printDetail=FALSE,printBar=FALSE,   # fixed part
            nT=C$nT,nD=C$nD,initT=C$initT,alpha=C$alpha) # irace
  res=SAopt(rastrigin,algo) # call SAopt
  return(list(cost=res$OFvalue)) # output list required by irace
}

scenario=list(targetRunner=rastrigin_run,
              instances=1, # not used but needs to be defined
              maxExperiments=200, # 200 calls to targetRunner  
              logFile = "") # do not create log file
# ------------------------------------------------------------

# run SAopt with default values:
cat("default SAopt:\n")
algo1=list(x0=par,nI=100,neighbour=rchange,printDetail=FALSE,printBar=FALSE)
res=SAopt(rastrigin,algo1) # call SAopt
cat(" evaluation value:",res$OFvalue,"\n")

# run irace:
cat("irace SAopt:\n")
s=irace(scenario=scenario,parameters=parameters)
# show best configurations:
configurations.print(s)
# get best
b=removeConfigurationsMetaData(s[1,])
print(b)
algo2=list(x0=par,nI=100,neighbour=rchange,
           printDetail=FALSE,printBar=FALSE,
           nT=b$nT,initT=b$initT,alpha=b$alpha)
res=SAopt(rastrigin,algo2) # call SAopt
cat(" evaluation value:",res$OFvalue,"\n")

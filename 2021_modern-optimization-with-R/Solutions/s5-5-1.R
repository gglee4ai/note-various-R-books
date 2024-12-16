# solution: s5-5-1.R
library(rgp) # load rgp

# auxiliary functions:
mae=function(y1,y2) mean(abs(y1-y2)) # mean absolute error function
eggholder=function(x) # length of x is 2
  -(x[2]+47)*sin(sqrt(abs(x[2]+x[1]/2+47)))-x[1]*sin(sqrt(abs(x[1]-(x[2]+47))))

fwrapper=function(x,f) 
{ res=suppressWarnings(f(x[1],x[2]))
  # if NaN is generated (e.g. sqrt(-1)) then
  if(is.nan(res)) res=Inf # replace by Inf
  return(res)
}

# configuration of the genetic programming:
ST=inputVariableSet("x1","x2")
cF1=constantFactorySet(function() sample(c(2,47),1) )
FS=functionSet("+","-","/","sin","sqrt","abs")
# set the input samples:
samples=100 
domain=matrix(ncol=2,nrow=samples)
domain[]=runif(samples,-512,512)
y=apply(domain,1,eggholder)
eval=function(f) # evaluation function
  mse(y,apply(domain,1,fwrapper,f))

# run the genetic programming:
gp=geneticProgramming(functionSet=FS,inputVariables=ST,
                      constantSet=cF1,populationSize=100,
                      fitnessFunction=eval,
                      stopCondition=makeStepsStopCondition(50),
                      verbose=TRUE)
# show the results:
b=gp$population[[which.min(gp$fitnessValues)]]
cat("best solution (f=",eval(b),"):\n")
print(b)
y2=apply(domain,1,fwrapper,b)
# sort L1 and L2 (according to L1 indexes)
# for an easier comparison of both curves:
MIN=min(y,y2);MAX=max(y,y2)
plot(y,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(y2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("eggholder","GP function"),lwd=2,lty=1:2)
# note: the fit is not perfect, but the search space is too large

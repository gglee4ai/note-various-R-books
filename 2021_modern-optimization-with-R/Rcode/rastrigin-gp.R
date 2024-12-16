### rastrigin-gp.R ###

library(rgp) # load rgp

# auxiliary functions:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
fwrapper=function(x,f) f(x[1],x[2]) # auxiliar function

# configuration of the genetic programming:
VS=inputVariableSet("x1","x2")
cF1=constantFactorySet(function() rnorm(1)) # mean=0, sd=1
FS=functionSet("+","*","-") # simple arithmetic operators
Pop=100
Gen=100
# simple monitor function: show best every 10 iterations:
monitor=function(population,objectiveVectors,fitnessFunction,
                 stepNumber,evaluationNumber,bestFitness,
                 timeElapsed,...)
{ if(stepNumber==2||stepNumber%%10==0)
    cat("iter:",stepNumber,"f:",bestFitness,"\n")
}

# set the input samples (grid^2 data points):
grid=10 # size of the grid used
domain=matrix(ncol=2,nrow=grid^2) # 2D domain grid
domain[,1]=rep(seq(-5.2,5.2,length.out=grid),each=grid)
domain[,2]=rep(seq(-5.2,5.2,length.out=grid),times=grid)

y=apply(domain,1,rastrigin) # compute target output

msevalue=function(f) # mse evaluation of function f
{ mse(y,apply(domain,1,fwrapper,f)) }

# run the genetic programming:
set.seed(12345) # set for replicability
mut=function(func) # set the mutation function
{ mutateSubtree(func,funcset=FS,inset=VS,conset=cF1,
                mutatesubtreeprob=0.1,maxsubtreedepth=4) }

# call the genetic programming:
gp=geneticProgramming(fitnessFunction=msevalue,
                      populationSize=Pop,
                      stopCondition=makeStepsStopCondition(Gen),
                      functionSet=FS,inputVariables=VS,
                      constantSet=cF1,mutationFunction=mut,
                      progressMonitor=monitor,verbose=TRUE)
# show the results:
b=gp$population[[which.min(gp$fitnessValues)]]
cat("best solution (f=",msevalue(b),"):\n")
print(b)

# create approximation pdf plot:
y2=apply(domain,1,fwrapper,b)
MIN=min(y,y2);MAX=max(y,y2)
pdf("gp-function.pdf",width=7,height=7,paper="special")
plot(y,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(y2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("rastrigin","GP function"),lwd=2,
       lty=1:2)
dev.off()

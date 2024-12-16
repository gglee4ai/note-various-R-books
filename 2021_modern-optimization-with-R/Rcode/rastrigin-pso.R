### rastrigin-pso.R file ###

library(pso)      # load pso::psoptim
library(psoptim)  # load psoptim::psoptim
library(NMOF)     # load PSopt

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
rastrigin2=function(x) -rastrigin(x)
D=30 

# common setup for the executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
Pop=20 # swarm size
Maxit=100 # maximum number of iterations

set.seed(12345) # set for replicability
Dig=2 # use 2 digits to show results

# simple function to show best solution and evaluation value:
showres=function(s,f,digits=Dig) 
 { cat("best:",round(s,Dig),"f:",round(f,Dig),"\n") }

cat("pso::psoptim:\n") # minimization
C=list(maxit=Maxit,s=Pop,type="SPSO2011") # SPSO2011
pso1=pso::psoptim(rep(NA,D),fn=rastrigin,
                  lower=lower,upper=upper,control=C)
showres(pso1$par,pso1$value)

cat("psoptim::psoptim:\n") # maximization
pso2=psoptim::psoptim(FUN=rastrigin2,
                      n=Pop, # swarm size
                      max.loop=Maxit,
                      xmin=lower,xmax=upper,
                      #velocity constraints in each direction:
                      vmax=rep(4,D), # default 4 value is used
                      anim=FALSE)
showres(pso2$sol,-pso2$val)

cat("PSopt:\n") # minimization
algo=list(nP=Pop,nG=Maxit,
          min=lower,max=upper, # lower and upper bounds
          printDetail=FALSE,printBar=FALSE)
pso3=PSopt(rastrigin,algo=algo)
showres(pso3$xbest,pso3$OFvalue)

### rastrigin-de.R file ###

library(DEoptim)  # load DEoptim
library(DEoptimR) # load JDEoptim
library(GA)       # load GA
library(NMOF)     # load DEopt

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
rastrigin2=function(x) -rastrigin(x) # maximization goal
D=30 

# common setup for the executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
Pop=20 # population size
Maxit=100 # maximum number of iterations
PCx=0.9 # fraction of values that are mutated
Df=0.8 # differential weighting factor

set.seed(12345) # set for replicability
Dig=2 # use 2 digits to show results

# simple function to show best solution and evaluation value:
showres=function(s,f,digits=Dig) 
 { cat("best:",round(s,Dig),"f:",round(f,Dig),"\n") }

cat("DEoptim:\n") # minimization
C=DEoptim.control(NP=Pop,itermax=Maxit,CR=PCx,F=Df,trace=FALSE)
de1=suppressWarnings(DEoptim(rastrigin,lower,upper,control=C))
showres(de1$optim$bestmem,de1$optim$bestval)

cat("JDEoptim:\n") # minimization
de2=suppressWarnings(JDEoptim(lower,upper,fn=rastrigin,
                     NP=Pop,maxiter=Maxit))
showres(de2$par,de2$value)

cat("de:\n") # maximization
de2=de(rastrigin2,lower,upper,popSize=Pop,maxiter=Maxit,
       pcrossover=PCx,stepsize=Df,
       monitor=FALSE)
showres(de2@solution,-de2@fitnessValue)

cat("DEopt:\n") # minimization
algo=list(nP=Pop,nG=Maxit,
          CR=PCx, # crossover probability
          F=Df, # default step size
          min=lower,max=upper, # lower and upper bounds
          printDetail=FALSE,printBar=FALSE)
de4=DEopt(rastrigin,algo=algo)
showres(de4$xbest,de4$OFvalue)

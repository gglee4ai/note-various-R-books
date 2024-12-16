### rastrigin-p1.R file ###

# packages with parallel execution:
library(GA)      # load ga
library(DEoptim) # load DEoptim

# package to measure time elapsed:
library(tictoc)	 # load tic and toc

# computationally slow rastrigin function:
srastrigin=function(x) 
{ res=10*length(x)+sum(x^2-10*cos(2*pi*x))
  Sys.sleep(0.1) # sleep ms
  return(res)
}
srastrigin2=function(x) -srastrigin(x) # maximization goal
D=30 # dimension 

# common setup for the executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
Pop=100 # population size
Maxit=20 # maximum number of iterations
Eli=1 # elitism of one individual 
Dig=2 # use 2 digits to show results

# show best solution, evaluation value and time elapsed:
showres2=function(f,tm,digits=Dig) 
 { cat("f:",round(f,Dig),"time:",round(tm,Dig),"\n")}

# Number of processing Cores (change if needed):
NC=4 # NC=detectCores() is another option
cat(">> Experiments with:",NC,"cores\n")

cat("sequential ga:\n")
set.seed(123) # set for replicability
tic()
ga1=ga(type="real-valued",fitness=srastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       popSize=Pop, # population size
       elitism=Eli,maxiter=Maxit,monitor=FALSE)
te=toc(quiet=TRUE) # time elapsed
showres2(-ga1@fitnessValue,te$toc-te$tic)

cat("parallel ga:\n")
set.seed(123) # set for replicability
tic()
ga2=ga(type="real-valued",fitness=srastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       popSize=Pop,elitism=Eli,maxiter=Maxit,
       parallel=NC, # NC cores for parallel evaluation
       monitor=FALSE)
te=toc(quiet=TRUE) # time elapsed
showres2(-ga2@fitnessValue,te$toc-te$tic)

cat("sequential DEoptim:\n") # minimization
set.seed(123) # set for replicability
tic()
C=DEoptim.control(NP=Pop,itermax=Maxit,trace=FALSE)
de1=suppressWarnings(DEoptim(srastrigin,lower,upper,control=C))
te=toc(quiet=TRUE) # time elapsed
showres2(de1$optim$bestval,te$toc-te$tic)

cat("parallel DEoptim:\n") # minimization
set.seed(123) # set for replicability
cl=makeCluster(NC) # set NC cores
tic()
C=DEoptim.control(NP=Pop,itermax=Maxit,trace=FALSE,parallelType=1,cluster=cl)
de2=suppressWarnings(DEoptim(srastrigin,lower,upper,control=C))
te=toc(quiet=TRUE) # time elapsed
stopCluster(cl) # stop the cluster
showres2(de2$optim$bestval,te$toc-te$tic)

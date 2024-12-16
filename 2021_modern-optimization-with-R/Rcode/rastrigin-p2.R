### rastrigin-p2.R file ###

library(GA)     # load ga and gaisl
library(tictoc) # load tic and toc

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
rastrigin2=function(x) -rastrigin(x) # maximization goal
D=30 

# common setup for the executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
Pop=1000 # population size
Maxit=20 # maximum number of iterations
Dig=2 # use 2 digits to show results
Migrate=5 # migrate solutions every 5 iterations

# show best solution, evaluation value and time elapsed:
showres2=function(f,tm,digits=Dig) 
 { cat("f:",round(f,Dig),"time:",round(tm,Dig),"\n")}

# Number of processing Cores (change if needed):
NC=detectCores()
cat(">> Experiments with:",NC,"cores\n")

cat("sequential ga:\n")
set.seed(123) # set for replicability
tic()
ga1=ga(type="real-valued",fitness=rastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       popSize=Pop, # population size
       maxiter=Maxit,monitor=FALSE)
te=toc(quiet=TRUE) # time elapsed
showres2(-ga1@fitnessValue,te$toc-te$tic)

cat("sequential island:\n")
set.seed(123) # set for replicability
tic()
ga2=gaisl(type="real-valued",fitness=rastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       popSize=Pop,
       numIslands=NC, # number of Islands
       migrationInterval=Migrate, 
       parallel=FALSE, # do not use 
       monitor=FALSE)
te=toc(quiet=TRUE) # time elapsed
showres2(-ga2@fitnessValue,te$toc-te$tic)

cat("islands ga (GA package):\n")
set.seed(123) # set for replicability
tic()
ga3=gaisl(type="real-valued",fitness=rastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       popSize=Pop,
       numIslands=NC, # number of Islands
       migrationInterval=Migrate, 
       parallel=TRUE, # use the cores 
       monitor=FALSE)
te=toc(quiet=TRUE) # time elapsed
showres2(-ga3@fitnessValue,te$toc-te$tic)

library(genalg) # load rba.bin
library(GA) # load ga
library(mcga) # load mcga

library(parallel) # load detectCores() and others
library(tictoc) # load tic and toc

D=2
# common setup for the executions:
lower=rep(-512,D);upper=rep(512,D) # lower and upper bounds
Pop=400 # population size
Gen=500 # maximum number of generations

# eggholder function:
eggholder=function(x) # length of x is 2
  -(x[2]+47)*sin(sqrt(abs(x[2]+x[1]/2+47)))-x[1]*sin(sqrt(abs(x[1]-(x[2]+47))))
eggholder2=function(x) -eggholder(x) # maximization goal
K=1000
eggholder3=function(x) # penalizes x values outside the [lower,upper] range
{ res=eggholder(x) #
  # death penalty if x is outside the [lower,upper] range:
  minx=min(x)
  maxx=max(x)
  if(minx<lower[1]||maxx>upper[1]) res=K
  return(res)
}

Dig=2 # use 2 digits to show results
# simple function to show best solution and evaluation value:
showres=function(s,f,digits=Dig) 
 { cat("best:",round(s,Dig),"f:",round(f,Dig),"\n") }

# rbga (minimization):
cat("rbga:\n")
tic()
ga1=rbga(lower,upper,popSize=Pop,iters=Gen,evalFunc=eggholder)
b=which.min(ga1$evaluations) # best individual 
# show best solution and evaluation value:
showres(ga1$population[b,],ga1$evaluations[b])
toc()

# gaisl (maximization):
cat("gaisl:\n")
NC=detectCores()
tic()
cat("gaisl with ",NC,"cores:\n")
ga2=gaisl(type="real-valued",fitness=eggholder2,
          lower=lower,upper=upper, # lower and upper bounds
          popSize=Pop,
          numIslands=NC, # number of Islands
          parallel=TRUE,
          maxiter=Gen,monitor=FALSE)
toc()
showres(ga2@solution,-ga2@fitnessValue)

# mcga (minimization):
cat("mcga:\n") # uniform binary crossover and bit mutation
tic()
ga3=mcga(popsize=Pop,chsize=D,
         minval=lower,maxval=upper,
         maxiter=Gen,evalFunc=eggholder3)
toc()
showres(ga3$population[1,],ga3$costs[1])

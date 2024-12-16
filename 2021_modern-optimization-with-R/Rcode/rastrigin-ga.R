### rastrigin-ga.R file ###

library(genalg)           # load rba.bin
library(ecr)    	  # load ecr
library(GA)     	  # load ga
library(mcga)	  	  # load mcga

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
rastrigin2=function(x) -rastrigin(x) # maximization goal
D=30 

# common setup for the executions:
lower=rep(-5.2,D);upper=rep(5.2,D) # lower and upper bounds
Pop=20 # population size
Gen=100 # maximum number of generations
Eli=1 # elitism of one individual 

set.seed(12345) # set for replicability
Dig=2 # use 2 digits to show results

# simple function to show best solution and evaluation value:
showres=function(s,f,digits=Dig) 
 { cat("best:",round(s,Dig),"f:",round(f,Dig),"\n") }

# rbga (minimization):
cat("rbga:\n")
ga1=rbga(lower,upper,popSize=Pop,iters=Gen,
         evalFunc=rastrigin,elitism=Eli)
b=which.min(ga1$evaluations) # best individual 
# show best solution and evaluation value:
showres(ga1$population[b,],ga1$evaluations[b])

# ecr (minimization):
cat("ecr:\n")
ga2=ecr(fitness.fun=rastrigin,minimize=TRUE,n.objectives=1,
        n.dim=D,
        lower=lower,upper=upper, # lower and upper bounds
        representation="float", # real values
        mu=Pop, # population size
        lambda=round(Pop/2), # half the population size
        n.elite=Eli, # elitism
        # tournament with k=2
        parent.selector=setup(selTournament,k=2),
        survival.selector=setp(selTournament,k=2), 
        recombinator=recUnifCrossover, # uniform crossover
        mutator=setup(mutGauss,lower=lower,upper=upper),
        terminators=list(stopOnIters(Gen)))
showres(ga2$best.x[[1]],ga2$best.y)

# ga (maximization):
cat("ga:\n")
ga3=ga(type="real-valued",fitness=rastrigin2,
       lower=lower,upper=upper, # lower and upper bounds
       selection=ga_tourSelection, # tournament with k=3
       crossover=gabin_uCrossover, # uniform crossover
       mutation=gareal_raMutation, # uniform random mutation
       popSize=Pop, # population size
       elitism=Eli,maxiter=Gen,monitor=FALSE)
showres(ga3@solution,-ga3@fitnessValue)

# mcga (minimization):
cat("mcga:\n") # uniform binary crossover and bit mutation
ga4=mcga(popsize=Pop,chsize=D,elitism=Eli,
         minval=lower,maxval=upper,
         maxiter=Gen,evalFunc=rastrigin)
showres(ga4$population[1,],ga4$costs[1])

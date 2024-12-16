### maxsin-ga.R ###

library(genalg) # load rba.bin
library(ecr)    # load ecr
library(GA)     # load GA
library(NMOF)   # load GAopt

# maxsin task:
intbin=function(x) sum(2^(which(rev(x==1))-1))
maxsin=function(x) sinpi(intbin(x)/(2^D)) # maximization goal
maxsin2=function(x) -maxsin(x)            # minimization goal
D=20 # number of dimensions

# GA parameter setup:
Pop=20 # population size
Gen=100 # maximum number of generations
Eli=1L # elitism of one individual
PCx=1.0 # crossover probability for a solution
PMut1=0.01 # single bit mutation probability
PMut2=0.10  # mutation probability for a solution  

# rbga.bin (minimization):
cat("rbga.bin:\n")
ga1=rbga.bin(size=D,popSize=Pop,iters=Gen,
            mutationChance=PMut1,zeroToOneRatio=1,
            evalFunc=maxsin2,elitism=Eli)
b=which.min(ga1$evaluations) # best individual 
# show best solution and evaluation value:
cat("best:",ga1$population[b,],"f:",-ga1$evaluations[b],"\n")

# ecr (maximization):
cat("ecr:\n")
ga2=ecr(maxsin,minimize=FALSE,n.objectives=1,
        n.dim=D,n.bits=D,representation="binary",
        mu=Pop, # population size
        lambda=(Pop-Eli), # new solutions in each generation
        p.recomb=PCx,p.mut=PMut2,
        n.elite=Eli,
        parent.selector=selRoulette, # roulette wheel
        survival.selector=selRoulette, # roulette wheel
        recombinator=recCrossover, # one-point crossover
        terminators = list(stopOnIters(Gen)))
cat("best:",ga2$best.x[[1]],"f:",ga2$best.y,"\n")

# ga (maximization):
cat("ga:\n")
ga3=ga(type="binary",maxsin,nBits=D,
       selection=gabin_nlrSelection, # nonlinear rank 
       crossover=gabin_spCrossover, # one-point crossover
       popSize=Pop,pcrossover=PCx,pmutation=PMut2,
       elitism=Eli,maxiter=Gen,monitor=FALSE)
cat("best:",ga3@solution,"f:",ga3@fitnessValue,"\n")

# GAopt (minimization):
cat("GAopt:\n")
algo=list(nB=D,nP=Pop,nG=Gen,
          crossover="onePoint", # one-point crossover
          prob=PMut1,printDetail=FALSE,printBar=FALSE)
ga4=GAopt(maxsin2,algo=algo)
cat("best:",as.numeric(ga4$xbest),"f:",-ga4$OFvalue,"\n")

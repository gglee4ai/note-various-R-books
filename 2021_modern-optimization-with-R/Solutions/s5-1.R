library(genalg) # load genalg
library(ecr)    # load ecr
library(GA)     # load GA
library(NMOF)   # load GAopt

# sum of bits:
maxsumbin=function(x) sum(x)
minsumbin=function(x) (length(x)-sum(x))

D=20 # number of dimensions

# GA parameter setup:
Pop=20 # population size
Gen=100 # maximum number of generations
Eli=1L # elitism of one individual

# auxiliary function:
showres=function(x) 
{ cat("x:",x,"f:",maxsumbin(x),"\n") }

# rbga.bin (minimization):
cat("rbga.bin:\n")
ga1=rbga.bin(size=D,popSize=Pop,iters=Gen,
            evalFunc=minsumbin,elitism=Eli)
b=which.min(ga1$evaluations) # best individual 
# show best solution and evaluation value:
showres(ga1$population[b,])

# ecr (maximization):
cat("ecr:\n")
ga2=ecr(maxsumbin,minimize=FALSE,n.objectives=1,
        n.dim=D,n.bits=D,representation="binary",
        mu=Pop, # population size
        lambda=(Pop-Eli), # new solutions in each generation
        n.elite=Eli,
        terminators = list(stopOnIters(Gen)))
showres(ga2$best.x[[1]])

# ga (maximization):
cat("ga:\n")
ga3=ga(type="binary",maxsumbin,nBits=D,
       popSize=Pop,elitism=Eli,maxiter=Gen,monitor=FALSE)
showres(ga3@solution)

# GAopt (minimization):
cat("GAopt:\n")
algo=list(nB=D,nP=Pop,nG=Gen,
          printDetail=FALSE,printBar=FALSE)
ga4=GAopt(minsumbin,algo=algo)
showres(as.numeric(ga4$xbest))

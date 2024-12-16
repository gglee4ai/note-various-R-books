### pareto-test2.R file ###

source("mo-tasks.R") # load multi-optimization tasks

library(mco) # load mco::nsga2
library(ecr) # load ecr::nsga2
library(caRamel) # load caRamel

m=2 # two objectives
Seed=12345
Gen=100
Pop=20
Maxit=100
Eli=1
D=8 # dimension
lower=rep(0,D);upper=rep(1,D)

cat("real value task:\n")
s1=nsga2(fn=fes1,idim=D,odim=m,
        lower.bounds=upper,upper.bounds=upper,
        popsize=Pop,generations=Gen)
p1=s1$value[s1$pareto.optimal,] # Pareto front: f1,f2

cat("run caRamel\n")
bounds=matrix(data=c(lower,upper),nrow=D,ncol=m)
prec=matrix(c(0.03,0.03),nrow=1,ncol=m) # desired point
s5=caRamel(nobj=m,nvar=D,minmax=c(FALSE,FALSE), # minmax=(min,min)
           bounds=bounds,func=fes1,
           popsize=Pop,archsize=Pop, # size of Pareto front
           carallel=FALSE,maxrun=Maxit,prec=prec,graph=FALSE)
p5=matrix(nrow=nrow(s5$total_pop),ncol=m)
for(i in 1:nrow(p5)) p5[i,]=ieval(s5$total_pop[i,1:D])

### perm.R file ###

#-- functions to handle permutation optimization tasks --

library(ecr)

# operators for permutation solutions:

### mutation operators:
exchange=function(s,N=length(s))
{ p=sample(1:N,2) # select two positions
  temp=s[p[1]] # swap values
  s[p[1]]=s[p[2]]
  s[p[2]]=temp
  return(s)
}

insertion=function(s,N=length(s),p=NA,i=NA)
{ if(is.na(p)) p=sample(1:N,1) # select a position
  I=setdiff(1:N,p) # ALL except p
  if(is.na(i)) i=sample(I,1) # select random place
  if(i>p) i=i+1 # need to produce a change
  I1=which(I<i) # first part
  I2=which(I>=i) # last part
  s=s[c(I[I1],p,I[I2])] # new solution
  return(s)
}

displacement=function(s,N=length(s))
{ p=c(1,N)
  # select random tour different than s
  while(p[1]==1&&p[2]==N) p=sort(sample(1:N,2))
  I=setdiff(1:N,p[1]:p[2]) # ALL except p
  i=sample(I,1) # select random place
  I1=which(I<i) # first part
  I2=which(I>=i) # last part
  s=s[c(I[I1],p[1]:p[2],I[I2])]
  return(s)
}

scramble=function(s,N=length(s))
{ p=c(1,N)
  # select random tour different than s
  while(p[1]==1&&p[2]==N) p=sort(sample(1:N,2))
  # scramble p
  scr=sample(p[1]:p[2],p[2]-p[1]+1)
  I=setdiff(1:N,p[1]:p[2]) # ALL except p
  I1=which(I<p[1]) # first part
  I2=which(I>p[2]) # last part
  s=s[c(I[I1],scr,I[I2])]
  return(s)
}

### crossover operators:
# partially matched crossover (PMX) operator:
# m is a matrix with 2 parent x ordered solutions
pmx=function(m)
{ N=ncol(m)
  p=sample(1:N,2) # two cutting points
  c=m # children
  for(i in p[1]:p[2])
   { # rearrange:
     c[1,which(c[1,]==m[2,i])]=c[1,i]
     # crossed section:
     c[1,i]=m[2,i]
     # rearrange:
     c[2,which(c[2,]==m[1,i])]=c[2,i]
     # crossed section:
     c[2,i]=m[1,i]
   } 
  return(c)
}

# order crossover (OX) operator:
# m is a matrix with 2 parent x ordered solutions
ox=function(m)
{ N=ncol(m)
  p=sort(sample(1:N,2)) # two cutting points
  c=matrix(rep(NA,N*2),ncol=N)
  # keep selected section:
  c[,p[1]:p[2]]=m[,p[1]:p[2]]
  # rotate after cut 2 (p[2]):
  I=((p[2]+1):(p[2]+N))
  I=ifelse(I<=N,I,I-N)
  a=m[,I]
  # fill remaining genes:
  a1=setdiff(a[2,],c[1,p[1]:p[2]])
  a2=setdiff(a[1,],c[2,p[1]:p[2]])
  I2=setdiff(I,p[1]:p[2])
  c[,I2]=rbind(a1,a2)
  return(c)
}

# auxiliary functions that:
# return same argument OR
#  population or fitness fields (if Lamarck evaluation)
pop_eF=function(pop,fitness)
{ if(!is.list(fitness)) return (pop)
  else # is.list (Lamarck)
   {
    for(i in 1:length(pop))
       pop[[i]]=fitness[,i]$sol
    return(pop)
   }
}
fit_eF=function(fitness)
{ 
  if(!is.list(fitness)) return (fitness)
  else # is.list (Lamarck)
   {
    res=matrix(ncol=ncol(fitness),nrow=1)
    for(i in 1:ncol(fitness)) res[,i]=fitness[,i]$eval
    return(res)
   }
}

# evolutionary algorithm with Lamarckian optimization 
#  uses the same arguments as ecr():
#  mu -- population size (if numeric) 
#     or initial population (if vector list)
#  perm -- the maximum integer size of the permutation
#  trace -- if >0, then the best is shown every trace iter. 
#  fitness.fun might perform a Lamarckian change, if it
#    does, then it should return a list with $eval and $sol
eal=function(fitness.fun,mu,lambda,perm=perm,
             p.recomb=0.7,p.mut=0.3,n.elite=0L,maxit,trace=maxit)
{ 
 # control object: fitness.fun,mutation,crossover,selection
 control=initECRControl(fitness.fun=fitness.fun,n.objectives=1)
 control=registerECROperator(control,"mutate",mutInsertion)
 control=registerECROperator(control,"recombine",recOX)
 # use roulette wheel for selection of individuals and parents:
 control=registerECROperator(control,"selectForSurvival",
                             selTournament)  
 control=registerECROperator(control,"selectForMating",
                             selTournament)
 
 if(is.numeric(mu)) pop=genPerm(mu,perm) # initial population
 else { pop=mu;mu=length(mu) } # use mu as initial population
 eF=evaluateFitness(control,pop) # evaluation
 pop=pop_eF(pop,eF) # update (if Lamarck)
 fitness=fit_eF(eF) # update (if Lamarck), matrix (1 x mu)

 if(trace>0) cat("initial pop. best:",min(fitness),"\n")
 for (i in seq_len(maxit)) # main evolutionary cycle 
   { 
    # sample lambda individuals:
    idx=sample(1:mu,lambda)
    # create and evaluate offspring:
    fitidx=matrix(fitness[,idx],ncol=lambda) 
    offspring=generateOffspring(control,pop[idx],fitness=fitidx,
                                lambda=lambda,p.recomb=0.7,p.mut=0.3)
    fitness.o=evaluateFitness(control,offspring)
    offspring=pop_eF(offspring,fitness.o) # update (if Lamarck)
    fitness.o=fit_eF(fitness.o) # get (if Lamarck)
    # selection of best solutions: 
    sel=replaceMuCommaLambda(control,pop,offspring,fitness,
                             fitness.o,n.elite=n.elite)
    pop=sel$population # update population
    fitness=sel$fitness # update fitness
    if(i%%trace==0) cat("gen:",i,"best:",min(fitness),"\n")
   }
 # pop is a vector list of size mu, fit is a matrix (1 x mu)
 return(list(pop=pop,fit=fitness))
}

### fes1-pareto.R file ###

source("mo-tasks.R") # load multi-optimization tasks

library(mco) # load dominatedHypervolume
library(ecr) # load nsga2, smsemoa
# comment this line if you cannot install MaOEA
library(MaOEA) # load optimMaOEA 
library(tictoc) # load tic and toc

# --- FES1 real value task (common setup):
m=2 # two objectives
D=8 # dimension
Pop=20 # population size
Gen=100 # maximum number of generations (stop criterion)
lower=rep(0,D);upper=rep(1,D) # lower and upper
Seed=12345 # random seed
ref=c(2.5,1.5) # reference point: used by the hypervolume
runs=20 # total number of execution runs

# auxiliary functions --------------------------------
# function that sorts m according to 1st column
o1=function(m) # m is a matrix
  return(m[order(m[,1]),])

# wilcox estimated median and confidence intervals:
wmedian=function(x,level=0.95) # x is a vector
{ # ignore wilcox.test warnings if they appear
  wx=suppressWarnings(wilcox.test(x,conf.level=level,
      conf.int=TRUE,alternative="two.sided",correct=TRUE))
  # use median if constant data in x:
  if(is.nan(wx$estimate)) wx$estimate=median(x)
  wx$conf.int[is.nan(wx$conf.int)]=c(wx$estimate,wx$estimate) 
  lint=wx$estimate-wx$conf.int[1] # low conf. int. value 
  uint=wx$conf.int[2]-wx$estimate # upper c.i. value 
  return(c(wx$estimate,lint,uint)) # median,lower c.i.,upper c.i.
}

# create a vertical averaging curve with vsamples on x-axis:
#   Pareto is a vector list with runs results
#   xlim and ylim are the plot x-axis and y-axis limits
#   samples is the number of x-axis samples
vmedian=function(curves,xlim,ylim,samples=15)
{
 # target x-axis equally spaced range:
 xout=seq(xlim[1],xlim[2],length.out=samples)
 m=matrix(nrow=length(curves),ncol=samples)

 for(i in 1:length(curves)) # cycle all curves
 { 
  # interpolate each curve to have points at xout values
  # (assumes min f1 and min f2 goals)
  acurve=suppressWarnings(approx(curves[[i]][,1],
          curves[[i]][,2],
          xout=xout,yleft=ylim[2],yright=ylim[1]))
  m[i,]=acurve$y # update the curve
 }
 vmed=matrix(nrow=4,ncol=samples)
 vmed[1,]=xout # first row
 # fill other rows with median,lower conf.int,upper conf.int
 for(j in 1:samples) # vertical average
 { 
   vmed[2:4,j]=wmedian(m[,j])
 }
 return(vmed) # vertical median curve
}

# plot a vertical confidence interval bar (from file compare.R):
#   x are the x-axis points
#   ylower and yupper are the lower and upper y-axis points
#   ... means other optional plot parameters (lty, etc.)
confbar=function(x,ylower,yupper,K=100,...)
{ segments(x-K,yupper,x+K,...)
  segments(x-K,ylower,x+K,...)
  segments(x,ylower,x,yupper,...)
}

methods=c("nsga2","smsemoa","nsga3")
# methods=c("nsga2","smsemoa","nsga3") # uncomment if no MaOEA
nm=length(methods) # shorter variable

# wrapper function that runs a MOEA, returns a Pareto front
moeatest=function(method,fn,m,D,Pop,ref,lower,upper,Gen)
{
 if(method=="nsga2")  
  { # NSGA-II
   s=ecr::nsga2(fitness.fun=fn,minimize=TRUE,n.objectives=m,
                n.dim=D,lower=lower,upper=upper,mu=Pop, 
                terminators=list(stopOnIters(Gen)))
   # convert to a f1 sorted matrix:
   return(as.matrix(o1(s$pareto.front)))
  }
 else if(method=="smsemoa")
  { # SMS-EMOA
   s=smsemoa(fitness.fun=fn,minimize=TRUE,n.objectives=m,n.dim=D,
          lower=lower,upper=upper,mu=Pop,ref.point=ref,
          terminators=list(stopOnIters(Gen)))
   # convert to a f1 sorted matrix:
   return(as.matrix(o1(s$pareto.front))) 
  }
 # comment this "else if" block if MaOEA is not installed -
 else if(method=="nsga3") # SBX and polynomial mutation
  { # NSGA-III
   # define the initial random population:
   pop=matrix(runif(Pop*D,min=lower[1],max=upper[1]),nrow=D)
   # control list NSGAIII defaults: pcx=1.0, pmut=1/D
   C=list(crossoverProbability=1.0,mutationProbability=1/D)
   s=optimMaOEA(x=pop,fun=fes1,solver=NSGA3,nObjective=m,
                nGeneration=Gen,seed=Seed,control=C)
   # convert to a f1 sorted matrix:
   return(as.matrix(o1(t(s$y))))
  }
 # end of #else if" block ---------------------------------
}

# execute several runs of a MOEA optimization:
runtest=function(runs,Seed,method,fn,m,D,Pop,ref,lower,upper,Gen)
{
 set.seed(Seed) # set for replicability
 s=sample(1:Seed,runs) # seed for each run
 res=vector("list",runs)
 tic()
 for(i in 1:runs)
 {
  set.seed(s[i]) # seed for the run
  Pareto=moeatest(method,fn,m,D,Pop,ref,lower,upper,Gen)
  res[[i]]=Pareto 
 }
 toc()
 return(res)
} 

res=vector("list",runs) # store all results in res
hv=matrix(nrow=nm,ncol=runs) # hypervolume results
for(i in 1:nm) 
{
 cat("execute ",runs,"runs with ",methods[i],"...\n")
 res[[i]]=runtest(runs,Seed,methods[i],fes1,m,D,Pop,ref,lower,
                  upper,Gen)
 # store all hypervolume run results for methods[i]:
 hv[i,]=unlist(lapply(res[[i]],dominatedHypervolume,ref))
 # show hypervolume median and confidence intervals:
 wm=round(wmedian(hv[i,]),2) # median and confidence intervals
 cat("median hypervolume: ",wm[1]," +- (",
     wm[2],",",wm[3],")\n",sep="")
}

xlim=c(0.7,5) # set manually for fes1
ylim=c(0,1.5) # set manually for fes1

# create pdf with all Pareto runs for all methods
pdf("fes1-all.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
# set an empty plot frame:
plot(res[[1]][[1]],xlim=xlim,ylim=ylim,type="n",
     xlab="f1",ylab="f2")
col=paste("gray",round(seq(1,50,length.out=nm)))
#col=c("black","blue","red") # if you prefer color
lwd=c(1,1,2)
for(i in 1:nm) # cycle methods
{ 
 for(r in 1:runs) # cycle runs
 { # plot each Pareto curve for each run
  lines(res[[i]][[r]],type="l",col=col[i],lty=i,lwd=lwd[i])
 }
}
legend("topright",methods,col=col,lty=1:nm,lwd=lwd)
dev.off()

# create pdf to compare Pareto front curves:
#   method proposed in (Cortez et al., 2020)
pdf("fes1-median.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
# set an empty plot frame:
plot(res[[1]][[1]],xlim=xlim,ylim=ylim,type="n",
     xlab="f1",ylab="f2")
samples=15
pch=1:nm
lwd=c(2,2,2)
K=diff(range(xlim))/samples/4 # 1/4 of the x-axis sample spacing
for(i in 1:nm)
{
 V=vmedian(res[[i]],xlim,ylim,samples=samples)
 # if needed clean artificially generated points (in extremes)
 if(i==2) ix=6:(samples-1) # set manually
 else if(i==3) ix=1:6 # set manually
 else ix=1:samples # all x-axis samples 
 lines(V[1,ix],V[2,ix],type="b",pch=pch[i],col=col[i],lwd=lwd[i])
 #plotH(V[1,ix],V[2,ix],V[3,ix],V[4,ix],col=col[i])
 confbar(V[1,ix],V[2,ix]-V[3,ix],V[2,ix]+V[4,ix],K=K,col=col[i])
}
legend("topright",methods,col=col,lty=1,lwd=lwd,pch=pch)
dev.off()

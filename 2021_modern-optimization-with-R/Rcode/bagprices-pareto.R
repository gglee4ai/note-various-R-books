### bagprices-pareto.R file ###

source("mo-tasks.R") # load multi-optimization tasks

library(mco) # load mco::nsga2
library(ecr) # load ecr::nsga2, smsemoa, asemoa

# --- integer bag prices task (common setup):
m=2 # two objectives
D=5 # 5 bag prices 
Pop=20 # population size
Gen=100 # maximum number of generations (stop criterion)
lower=rep(1,D);upper=rep(1000,D)
Seed=12345
ref=c(0,140) # reference point: used by the hypervolume

# auxiliary functions --------------------------------
# ieval: integer evaluation (minimization goal):
ieval=function(x) c(-profit(x),produced(x)) # -f1,f2
# function that converts -f1 to f1:
f1=function(m) # m is a matrix
{ m[,1]= -1*m[,1]; return(m) } # f1 = -1 * -f1

# function that sorts m according to 1st column
o1=function(m) # m is a matrix
  return(m[order(m[,1]),])
# function that shows the hypervolume 
showhv=function(p) # p is a Pareto front, uses global ref
 { hv=dominatedHypervolume(as.matrix(p),ref) # compute hypervolume
   cat("hypervolume=",hv,"\n")
 }
# ----------------------------------------------------

methods=c("mco::nsga2","ecr::nsga2","ecr::smsemoa","ecr:asemoa",
          "reference","aspiration")
cat("run ",methods[1],"\n")
set.seed(Seed)
s1=mco::nsga2(fn=ieval,idim=5,odim=m,
         lower.bounds=lower,upper.bounds=upper, # bounds
         popsize=Pop,generations=Gen)
p1=s1$value[s1$pareto.optimal,]
showhv(p1)
p1=o1(f1(p1)) # Pareto front: f1,f2

cat("run ",methods[2],"\n")
set.seed(Seed)
s2=ecr::nsga2(fitness.fun=ieval,minimize=TRUE,n.objectives=m,
              n.dim=D,lower=lower,upper=upper,mu=Pop, 
              terminators=list(stopOnIters(Gen)))
p2=s2$pareto.front # convert to matrix
showhv(p2)
p2=o1(f1(p2)) # Pareto front: f1,f2

cat("run ",methods[3],"\n")
set.seed(Seed)
s3=smsemoa(fitness.fun=ieval,minimize=TRUE,n.objectives=m,
           n.dim=D,
           lower=lower,upper=upper,mu=Pop,ref.point=ref,
           terminators=list(stopOnIters(Gen)))
p3=s3$pareto.front # convert to matrix
showhv(p3)
p3=o1(f1(p3)) # Pareto front: f1,f2

cat("run ",methods[4],"\n")
set.seed(Seed)
# set the aspiration set (in this case, Pop points are used):
# basic aspiration with 2 lines and 1 inflection point
p0=matrix(ncol=Pop,nrow=m)
half1=round(Pop/2);half2=Pop-half1 # for 2 sequences
p0[1,]=c(seq(-5000,-30000,length.out=half1),
         seq(-30000,-45000,length.out=half2)) 
p0[2,]=c(seq(0,30,length.out=half1),
         seq(30,100,length.out=half2))
s4=asemoa(fitness.fun=ieval,minimize=TRUE,n.objectives=m,
          n.dim=D,aspiration=p0,
          lower=lower,upper=upper,mu=Pop,
          terminators=list(stopOnIters(Gen)))
p4=s4$pareto.front # convert to matrix
showhv(p4)
p4=o1(f1(p4)) # Pareto front: f1,f2

pdf("bagprices-pareto.pdf")
par(mar=c(4.0,4.0,0.1,0.1)) # set pdf margins
xlim=c(-1,45000);ylim=c(-1,141) # plot x and y limits
plot(p1,xlim=xlim,ylim=ylim,xlab="f1",ylab="f2",type="n")
col=c(paste("gray",round(seq(1,75,length.out=5)),sep=""),"black")
pch=c(1:3,5,4,18) # types of points used
# plot the 4 Pareto fronts:
lines(p1,type="b",cex=0.5,lwd=2,lty=1,pch=pch[1],col=col[1])
lines(p2,type="b",cex=0.5,lwd=2,lty=2,pch=pch[2],col=col[2])
lines(p3,type="b",cex=0.5,lwd=2,lty=3,pch=pch[3],col=col[3])
lines(p4,type="b",cex=0.5,lwd=2,lty=4,pch=pch[4],col=col[4])
# show reference point:
points(ref[1],ref[2],pch="X",cex=1.0,col=col[5])
abline(v=ref[1],col=col[5])
abline(h=ref[2],col=col[5])
# show aspiration set
p0=t(p0) 
p0=f1(p0)
lines(p0,type="b",cex=1.0,lwd=1,lty=1,pch=pch[6],col=col[6])
legend("bottomright",methods,lwd=c(rep(2,4),1,1),
       pch=pch,lty=c(1:4,1,1),col=col)
dev.off()

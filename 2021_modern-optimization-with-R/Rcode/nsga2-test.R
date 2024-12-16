### nsga2-test.R file ###

source("mo-tasks.R") # load multi-optimization tasks
library(mco) # load mco package

set.seed(12345) # set for replicability
m=2 # two objectives

# --- binary task:
D=8 # 8 bits
# eval: transform binary objectives into minimization goal
#       round(x) is used to convert real number to 0 or 1 values 
beval=function(x) c(-sumbin(round(x)),-maxsin(round(x)))

cat("binary task:\n")
G=nsga2(fn=beval,idim=D,odim=m,
        lower.bounds=rep(0,D),upper.bounds=rep(1,D),
        popsize=12,generations=100)
# show last Pareto front
I=which(G$pareto.optimal)
for(i in I)
{
 x=round(G$par[i,])
 cat(x," f=(",sumbin(x),",",round(maxsin(x),2),")","\n",sep="")
}

# --- integer task:
D=5 # 5 bag prices 
# ieval: integer evaluation (minimization goal):
ieval=function(x) c(-profit(x),produced(x))
# function that sorts matrix m according to 1st column
o1=function(m) # m is a matrix
  return(m[order(m[,1]),])

cat("integer task:\n")
G=nsga2(fn=ieval,idim=5,odim=m,
        lower.bounds=rep(1,D),upper.bounds=rep(1000,D),
        popsize=20,generations=1:100)
# show best individuals:
I=which(G[[100]]$pareto.optimal)
for(i in I)
{
 x=round(G[[100]]$par[i,])
 cat(x," f=(",profit(x),",",produced(x),")","\n",sep=" ")
}
# create PDF with Pareto front evolution:
pdf(file="nsga-bag.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1))
I=1:100
for(i in I)
{ P=G[[i]]$value # objectives f1 and f2
  P[,1]=-1*P[,1] # show positive f1 values
  # color from light gray (75) to dark (1):
  COL=paste("gray",round(76-i*0.75),sep="")
  if(i==1) plot(P,xlim=c(-500,44000),ylim=c(0,140),
                xlab="f1",ylab="f2",cex=0.5,col=COL)
  Pareto=P[G[[i]]$pareto.optimal,]
  # sort Pareto according to x axis:
  Pareto=o1(Pareto) 
  points(P,type="p",pch=1,cex=0.5,col=COL)
  lines(Pareto,type="l",cex=0.5,col=COL)
}
dev.off()

# create PDF comparing NSGA-II with WF:
pdf(file="nsga-bag2.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1))
# NSGA-II best results:
P=G[[100]]$value # objectives f1 and f2
P[,1]=-1*P[,1] # show positive f1 values
Pareto=P[G[[100]]$pareto.optimal,]
# sort Pareto according to x axis:
Pareto=o1(Pareto)
plot(Pareto,xlim=c(-500,44000),ylim=c(0,140),
     xlab="f1",ylab="f2",type="b",lwd=2,lty=1,pch=1)
# weight-formula (wf) best results:
wf=read.table("wf-bag.csv",sep=" ") # data.frame
# paretoFilter only works with minimization goals:
wf=as.matrix(cbind(-wf[,1],wf[,2])) # matrix with -f1,f2
pwf=paretoFilter(wf) # get the Pareto front points of wf
wf[,1]=-wf[,1] # set to the f1,f2 domain
pwf[,1]=-pwf[,1] # set to the f1,f2 domain
points(wf,pch=3,lwd=2) # plot all wf points
lines(pwf,type="l",lty=2,lwd=2)
legend("topleft",c("NSGA-II","weighted-formula"),
       lwd=2,lty=1:2,pch=c(1,3))
dev.off()

# --- real value task:
D=8 # dimension
cat("real value task:\n")
G=nsga2(fn=fes1,idim=D,odim=m,
        lower.bounds=rep(0,D),upper.bounds=rep(1,D),
        popsize=20,generations=1:100)
# show best individuals:
I=which(G[[100]]$pareto.optimal)
for(i in I)
{
 x=round(G[[100]]$par[i,],digits=2); cat(x)
 cat(" f=(",round(fes1(x)[1],2),",",round(fes1(x)[2],2),")","\n",sep="")
}
# create PDF with Pareto front evolution:
pdf(file="nsga-fes1.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1))
I=1:100
for(i in I)
{ P=G[[i]]$value # objectives f1 and f2
  # color from light gray (75) to dark (1):
  COL=paste("gray",round(76-i*0.75),sep="")
  if(i==1) plot(P,xlim=c(0.5,5.0),ylim=c(0,2.0),
                xlab="f1",ylab="f2",cex=0.5,col=COL)
  Pareto=P[G[[i]]$pareto.optimal,]
  # sort Pareto according to x axis:
  Pareto=o1(Pareto) 
  points(Pareto,type="p",pch=1,cex=0.5,col=COL)
  lines(Pareto,type="l",cex=0.5,col=COL)
}
dev.off()

# create PDF comparing NSGA-II with WF:
pdf(file="nsga-fes1-2.pdf",paper="special",height=5,width=5)
par(mar=c(4.0,4.0,0.1,0.1))
# NSGA-II best results:
P=G[[100]]$value # objectives f1 and f2
Pareto=P[G[[100]]$pareto.optimal,]
# sort Pareto according to x axis:
Pareto=o1(Pareto)
plot(Pareto,xlim=c(0.5,5.0),ylim=c(0,2.0),
     xlab="f1",ylab="f2",type="b",lwd=2,pch=1)
# weight-formula best results:
wf=read.table("wf-fes1.csv",sep=" ") # data.frame
wf=as.matrix(wf) # convert to matrix
pwf=paretoFilter(wf) # get the Pareto front points of wf
points(wf,pch=3,lwd=2) # plot all wf points
lines(pwf,type="l",lty=2,lwd=2)
legend("top",c("NSGA-II","weighted-formula"),
       lwd=2,lty=1:2,pch=c(1,3))
dev.off()

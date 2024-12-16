### tsp.R file ###

library(TSP) # load TSP package
library(RCurl) # load RCurl package
library(ecr) # load ecr package
source("perm.R") # load permutation operators

# get Qatar - 194 cities TSP instance:
txt=getURL("http://www.math.uwaterloo.ca/tsp/world/qa194.tsp")
# simple parse of txt object, removing header and last line:
txt=strsplit(txt,"NODE_COORD_SECTION") # split text into 2 parts
txt=txt[[1]][2] # get second text part
txt=strsplit(txt,"EOF") # split text into 2 parts
txt=txt[[1]][1] # get first text part
# save data into a simple .csv file, sep=" ":
cat(txt,file="qa194.csv")
# read the TSP format into Data 
# (first row is empty, thus header=TRUE)
# get city Cartesian coordinates

Data=read.table("qa194.csv",sep=" ")
Data=Data[,3:2] # longitude and latitude
names(Data)=c("x","y") # x and y labels
 # number of cities

# distance between two cities (EUC_2D-norm)
# Euclidean distance rounded to whole number
D=dist(Data,upper=TRUE)
D[1:length(D)]=round(D[1:length(D)])
# create TSP object from D:
TD=TSP(D)

set.seed(12345) # for replicability
cat("2-opt run:\n")
PTM=proc.time() # start clock
R1=solve_TSP(TD,method="2-opt") 
sec=(proc.time()-PTM)[3] # get seconds elapsed
print(R1) # show optimum
cat("time elapsed:",sec,"\n")

MAXIT=50000 # maximum number of evaluations
Methods=c("SANN","EA","EA2","LEA") # comparison of 4 methods

RES=matrix(nrow=MAXIT,ncol=length(Methods)) 
MD=as.matrix(D)
N=nrow(MD)

# overall distance of a tour (evaluation function):
tour=function(s) # s is a solution (permutation)
{ # compute tour length:
  EV<<-EV+1 # increase evaluations
  s=c(s,s[1]) # start city is also end city
  res=0
  for(i in 2:length(s)) res=res+MD[s[i],s[i-1]]
  # store in global memory the best values:
  if(res<BEST) BEST<<-res
  if(EV<=MAXIT) F[EV]<<-BEST
  # only for hybrid method:
  # return tour
  return(res)
}

# Lamarckian evaluation function:
#  uses 2-opt to improve solution s, returns a list with:
#  eval - the tour for improved solution
#  sol  - the improved solution
ltour2opt=function(s)
{
 EV<<-EV+1 # increase evaluations
 # improve s using "2-opt" method:
 s2=solve_TSP(TD,method="2-opt",control=list(tour=s))
 res=attr(s2,"tour_length") # solve_TSP computes the tour
 if(res<BEST) BEST<<-res
 if(EV<=MAXIT) F[EV]<<-BEST
 return( list(eval=res,sol=as.numeric(s2)) )
}

Trace=1000 # show at the console the optimization evolution

cat("SANN run:\n")
set.seed(12345) # for replicability
s=sample(1:N,N) # initial solution
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
C=list(maxit=MAXIT,temp=2000,trace=TRUE,REPORT=Trace)
PTM=proc.time() # start clock
SANN=optim(s,fn=tour,gr=insertion,method="SANN",control=C)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"best tour:",F[MAXIT],"\n")
RES[,1]=F

# EA: simple permutation based evolutionary algorithm
cat("EA run:\n")
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
popSize=30;lambda=round(popSize/2);Eli=1
maxit=ceiling((MAXIT-popSize)/(lambda-1))

PTM=proc.time() # start clock
EA=eal(fitness.fun=tour,mu=popSize,lambda=lambda,
        perm=N,n.elite=Eli, # elitism
        maxit=maxit,trace=Trace)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"best tour:",F[MAXIT],"\n")
RES[,2]=F

# EA2: initial 2-opt search + EA
cat("EA2 run:\n")
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
maxit=ceiling((MAXIT-popSize)/(lambda-1))
PTM=proc.time() # start clock
# apply 2-opt local search to initial population:
pop=genPerm(popSize,N) # create random population
control=initECRControl(fitness.fun=ltour2opt,n.objectives=1)
eF=evaluateFitness(control,pop)
pop=pop_eF(pop,eF) # update population
# run standard evolutionary algorithm
EA2=eal(fitness.fun=tour,mu=pop,lambda=lambda,
        perm=N,n.elite=Eli, # elitism
        maxit=maxit,trace=Trace)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"best tour:",F[MAXIT],"\n")
RES[,3]=F

cat("LEA run:\n")
popSize=30;lambda=round(popSize/2);Eli=1
set.seed(12345) # for replicability
EV=0; BEST=Inf; F=rep(NA,MAXIT) # reset these vars.
maxit=ceiling((MAXIT-popSize)/(lambda-1))
PTM=proc.time() # start clock
LEA=eal(fitness.fun=ltour2opt,mu=popSize,lambda=lambda,
        perm=N,n.elite=Eli, # elitism
        maxit=maxit,trace=Trace)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"best tour:",F[MAXIT],"\n")
RES[,4]=F

# create PDF with comparison:
pdf("qa194-opt.pdf",paper="special")
par(mar=c(4.0,4.0,0.1,0.1))
X=seq(1,MAXIT,length.out=200)
ylim=c(min(RES)-50,max(RES))
plot(X,RES[X,1],ylim=ylim,type="l",lty=4,lwd=2,
     xlab="evaluations",ylab="tour distance")
lines(X,RES[X,2],type="l",lty=3,lwd=2)
lines(X,RES[X,3],type="l",lty=2,lwd=2)
lines(X,RES[X,4],type="l",lty=1,lwd=2)
legend("topright",Methods,lwd=2,lty=4:1)
dev.off()

# create 3 PDF files with best tours:
pdf("qa194-2-opt.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
plot(Data[c(R1[1:N],R1[1]),],type="l",xaxt="n",yaxt="n")
dev.off()
pdf("qa194-ea.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
b=EA$pop[[which.min(EA$fit)]]
plot(Data[c(b,b[1]),],type="l",xaxt="n",yaxt="n")
dev.off()
pdf("qa194-lea.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
b=LEA$pop[[which.min(LEA$fit)]]
plot(Data[c(b,b[1]),],type="l",xaxt="n",yaxt="n")
dev.off()

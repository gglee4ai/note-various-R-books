### tsp2.R file ###
# this file assumes that tsp.R has already been executed

library(rgeos) # get gArea function

N2=25 # lets consider just 25 cities

# creates a polygon (rgeos package) object from TSP data
poly=function(data)
{ poly="";sep=", "
  for(i in 1:nrow(data))
  { if(i==nrow(data)) sep=""
    poly=paste(poly,paste(data[i,],collapse=" "),sep,sep="")
  }
  poly=paste("POLYGON((",poly,"))",collapse="")
  poly=readWKT(poly) # WKT format to polygon
}

# new evaluation function: area of polygon 
area=function(s) return(gArea(poly(Data[c(s,s[1]),])))

# new data with N2 cities:
Data2=Data[1:N2,]
D2=dist(Data2,upper=TRUE)
D2[1:length(D2)]=round(D2[1:length(D2)])
# create TSP object from D:
TD2=TSP(D2)
set.seed(12345) # for replicability
R2=solve_TSP(TD2,method="2-opt") 
cat("area of 2-opt TSP tour:",area(R2),"\n")

# plot area of 2-opt:
pdf("qa-2opt-area.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
PR1=poly(Data[c(R2,R2[1]),])
plot(PR1,col="gray")
dev.off()

# EA:
cat("EA run for TSP area:\n")
set.seed(12345) # for replicability
pSize=30;lambda=round(pSize/2);maxit=40;Eli=1
PTM=proc.time() # start clock
OEA=eal(fitness.fun=area,mu=popSize,lambda=lambda,
        perm=N2,n.elite=Eli, # elitism
        maxit=maxit,trace=maxit)
sec=(proc.time()-PTM)[3] # get seconds elapsed
bi=which.min(OEA$fit)
b=OEA$pop[[bi]]
cat("best fitness:",OEA$fit[1,bi],"time elapsed:",sec,"\n")

# plot area of EA best solution: 
pdf("qa-ea-area.pdf",paper="special")
par(mar=c(0.0,0.0,0.0,0.0))
PEA=poly(Data[c(b,b[1]),])
plot(PEA,col="gray")
lines(Data[c(b,b[1]),],lwd=2)
dev.off()

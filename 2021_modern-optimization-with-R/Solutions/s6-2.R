library(genalg) # load rbga function
library(mco)  # load nsga2 function

set.seed(12345) # set for replicability

# real value FES2 benchmark:
fes2=function(x)
{ D=length(x);f=rep(0,3)
  for(i in 1:D)
     {
      f[1]=f[1]+(x[i]-0.5*cos(10*pi/D)-0.5)^2
      f[2]=f[2]+abs(x[i]-(sin(i-1))^2*(cos(i-1)^2))^0.5
      f[3]=f[3]+abs(x[i]-0.25*cos(i-1)*cos(2*i-2)-0.5)^0.5
     }
  return(f) 
}

D=8;m=3

# WBGA execution:
# evaluation function for WBGA 
# (also used to print and get last population fes2 values:
# WBGA chromosome used: x=(w1,w2,w3,v1,v2,v3,...,vD)
#   where w_i are the weights and v_j the values
eval=function(x,REPORT=FALSE)
{ D=length(x)/2
  # normalize weights, such that sum(w)=1
  w=x[1:m]/sum(x[1:m]);v=x[(m+1):length(x)];f=fes2(v)
  if(REPORT)
    { cat("w:",round(w,2),"v:",round(v,2),"f:",round(f,2),"\n")
      return(f)
    }
  else return(sum(w*f))
}
WBGA=rbga(evalFunc=eval,
          stringMin=rep(0,D*2),stringMax=rep(1,D*2),
          popSize=20,iters=100)
print("WBGA last population:")
# S1 contains the Pareto curve: 20 solutions x 3 objectives
S1=t(apply(WBGA$population,1,eval,REPORT=TRUE)) 

# NSGA-II execution:
NSGA2=mco::nsga2(fn=fes2,idim=D,odim=m,
        lower.bounds=rep(0,D),upper.bounds=rep(1,D),
        popsize=20,generations=100)
# S2 contains the Pareto curve: 20 solutions x 3 objectives
S2=NSGA2$value[NSGA2$pareto.optimal,]
print("NSGA2 last Pareto front:")
print(round(S2,2))

# Comparison of results:
ref=c(2.0,8.0,10.0)
hv1=dominatedHypervolume(S1,ref)
cat("WGA hypervolume",round(hv1,2),"\n")
hv2=dominatedHypervolume(S2,ref)
cat("NSGA-II hypervolume",round(hv2,2),"\n")


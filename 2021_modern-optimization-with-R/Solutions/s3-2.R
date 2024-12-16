source("blind.R") # load the blind search methods
source("grid.R") # load the grid search methods
source("functions.R") # load the profit function

D=5 # number of dimensions
# grid search code:
S1=gsearch(profit,rep(350,D),rep(450,D),rep(11,D),"max") 
cat("gsearch s:",round(S1$sol),"f:",S1$eval,"\n")

# dfsearch code:
domain=vector("list",D) 
for(i in 1:D) domain[[i]]=seq(350,450,by=11)
S2=dfsearch(domain=domain,fn=profit,type="max")
cat("dfsearch s:",round(S2$sol),"f:",S2$eval,"\n")

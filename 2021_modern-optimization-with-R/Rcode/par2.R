### par2.R file ###
# parallel example with "parLapply" function:

# load the libraries:
library(parallel) 
library(tictoc)

n=detectCores() # attempt to detect the CPU cores 
cat("Set the cluster with",n,"cores.\n")
cl=makeCluster(n) # set the cores


V=vector("list",n)
set.seed(123) # set for replicability
s=sample(1:100000000,n) # variable sizes
for(i in 1:n) V[[i]]=1:s[i] # sequence

# serial execution:
tic("serial execution") 
lapply(V,mad)
toc()

# parallel execution:
tic("parallel execution:") 
parLapplyLB(cl,V,mad) # execute parallel cycle
toc() 

# load balancing parallel execution:
tic("LB parallel execution:") 
# load balancing parallel version:
parLapply(cl,V,mad) 
toc()

stopCluster(cl) # stop the cluster

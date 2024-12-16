### par1.R file ###
# parallel example with "parLapply" function:

# load the libraries:
library(parallel) 
library(tictoc)

cl=makeCluster(getOption("cl.cores", 2)) # set 2 cores
mysleep=function(s) { Sys.sleep(s) }     # my sleep function 
x=c(2,2) # in seconds

tic("single execution:") # set the timer
lapply(x,mysleep) # execute serial cycle
toc() # measure time elapsed

tic("parallel execution:") # set the timer
parLapply(cl,x,mysleep) # execute parallel cycle
toc() # measure time elapsed

stopCluster(cl) # stop the cluster

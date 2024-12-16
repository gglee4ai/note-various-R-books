### rastrigin-acor.R file ###

library(evoper) # load evoper

# real value rastrigin function with a dimension of 30:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
rastrigin3=function(...) # called as: rastrigin3(x1,x2,...,xD) 
{ # ... means a variable sequence of arguments
 args=as.list(match.call()) # get the ... arguments
 # args: rastrigin and the ... arguments
 # args[[1]]="rastrigin", thus use only 2:length(args)
 x=vector(length=length(args)-1)
 for(i in 2:length(args)) x[i-1]=as.numeric(args[[i]])
 return(rastrigin(x))
}
D=30 # dimension 
lower=-5.2;upper=5.2 # lower and upper bounds
f=PlainFunction$new(rastrigin3)
# set the 30 parameters with lower and upper bounds:
for(i in 1:30) 
  f$Parameter(name=paste("x",i,sep=""),min=lower,max=upper)

# set acor internal parameters:
opt=OptionsACOR$new() # get default acor options
opt$setValue("iterations",10)
opt$setValue("n.ants",64) # The number of simulated ants

set.seed(12345) # set for replicability

# run the ant colony optimization: 
aco=abm.acor(f,opt)
b=aco$overall.best # best solution
print(b)

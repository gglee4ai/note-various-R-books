# solution: s5-5-2.R
library(gramEvol) # load gramEvol

# auxiliary functions:
mae=function(y1,y2) mean(abs(y1-y2)) # mean absolute error function
eggholder=function(x) # length of x is 2
  -(x[2]+47)*sin(sqrt(abs(x[2]+x[1]/2+47)))-x[1]*sin(sqrt(abs(x[1]-(x[2]+47))))

# set the grammar rules:
ruleDef=list(expr=gsrule("<expr><op><expr2>","<func>(<expr>)","<expr2>"),
             op=gsrule("+","-","*"), 
             func=gsrule("sin","sqrt","abs"),
             expr2=gsrule("x[1]","x[2]","<value>"),
             value=gsrule("<digits>.<digits>"),
             digits=gsrule("<digits><digit>","<digit>"),
             digit=grule(0,1,2,3,4,5,6,7,8,9)
            )
# create the BNF grammar object:
gDef=CreateGrammar(ruleDef)

# grammatical evolution setup:
Pop=100 # population size
Gen=50 # number of generations
# simple monitoring function:
monitor=function(results)
{ # print(str(results)) shows all results components
  iter=results$population$currentIteration # current iteration 
  f=results$best$cost # best fitness value
  if(iter==1||iter%%10==0) # show 1st and every 10 iter
    cat("iter:",iter,"f:",f,"\n")
}

# set the input samples:
samples=100 
domain=matrix(ncol=2,nrow=samples)
domain[]=runif(samples,-512,512)
y=apply(domain,1,eggholder) # compute target output

K=1000 # large penalty value
eval1=function(x,expr) # x is an input vector with D=2
{ # expr can include x[1] or x[2] symbols 
  #print(expr)
  res=suppressWarnings(eval(expr)) # can generate NaNs
  if(is.nan(res)) res=K
  return(res)
}

maevalue=function(expr) # evaluation function
{ 
 y_expr=apply(domain,1,eval1,expr) # expr outputs for domain
 return (mae(y,y_expr))
}

set.seed(12345) # set for replicability
# run the grammar evolution:
ge=suppressWarnings(GrammaticalEvolution(gDef,maevalue,popSize=Pop,
                        iterations=Gen,monitorFunc=monitor))
b=ge$best # best solution
cat("evolved phenotype:")
print(b$expression)
cat("f:",b$cost,"\n")

# create approximation plot:
y2=apply(domain,1,eval1,b$expression)
MIN=min(y,L2);MAX=max(y,y2)
plot(y,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(y2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("eggholder","GE function"),lwd=2,
       lty=1:2)

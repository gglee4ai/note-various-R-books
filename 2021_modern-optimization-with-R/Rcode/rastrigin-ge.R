### rastrigin-ge.R ###

library(gramEvol) # load gramEvol

# auxiliary functions:
rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
mse=function(y1,y2) mean((y1-y2)^2) # mean squared error

# set the grammar rules:
#   the first production rule is:
#   <expr> ::= <expr><op><expr2> | <expr2>
ruleDef=list(expr=gsrule("<expr><op><expr2>","<expr2>"),
             op=gsrule("+","-","*"), 
             expr2=gsrule("x[1]","x[2]","<value>"),
             value=gsrule("<digits>.<digits>"),
             digits=gsrule("<digits><digit>","<digit>"),
             digit=grule(0,1,2,3,4,5,6,7,8,9)
            )
# create the BNF grammar object:
gDef=CreateGrammar(ruleDef)

# two expansion examples:
expr=GrammarMap(c(2,1,0,0,4),gDef,verbose=TRUE)
print(expr) # show expression: x[1] + x[2] 
expr2=GrammarMap(c(3,2,1,1,5,0,1,3,2),gDef,verbose=TRUE)
print(expr2) # show expression: 5.32

# grammatical evolution setup:
Pop=100 # population size
Gen=100 # number of generations
Eli=1 # elitism
# simple monitoring function:
monitor=function(results)
{ # print(str(results)) shows all results components
  iter=results$population$currentIteration # current iteration 
  f=results$best$cost # best fitness value
  if(iter==1||iter%%10==0) # show 1st and every 10 iter
    cat("iter:",iter,"f:",f,"\n")
}

# set the input samples (grid^2 data points):
grid=10 # size of the grid used
domain=matrix(ncol=2,nrow=grid^2) # 2D domain grid
domain[,1]=rep(seq(-5.2,5.2,length.out=grid),each=grid)
domain[,2]=rep(seq(-5.2,5.2,length.out=grid),times=grid)

y=apply(domain,1,rastrigin) # compute target output

eval1=function(x,expr) # x is an input vector with D=2
{ # expr can include x[1] or x[2] symbols 
  eval(expr) 
}

msevalue2=function(expr) # evaluation function
{ 
 y_expr=apply(domain,1,eval1,expr) # expr outputs for domain
 return (mse(y,y_expr))
}

set.seed(12345) # set for replicability
# run the grammar evolution:
ge=GrammaticalEvolution(gDef,msevalue2,optimizer="ga",
                        popSize=Pop,elitism=Eli,
                        iterations=Gen,monitorFunc=monitor)
b=ge$best # best solution
cat("evolved phenotype:")
print(b$expression)
cat("f:",b$cost,"\n")

# create approximation plot:
y2=apply(domain,1,eval1,b$expression)
MIN=min(y,y2);MAX=max(y,y2)
pdf("ge-function.pdf",width=7,height=7,paper="special")
plot(y,ylim=c(MIN,MAX),type="l",lwd=2,lty=1,
     xlab="points",ylab="function values")
lines(y2,type="l",lwd=2,lty=2)
legend("bottomright",leg=c("rastrigin","GE function"),lwd=2,
       lty=1:2)
dev.off()

## ----label=C15rastrigindef, echo=TRUE------------------------------------
Rastrigin <- function(x) {
       sum(x^2 - 10 * cos(2 * pi  * x)) + 10 * length(x)
}
dimension <- 5
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)
start1 <- rep(1, dimension)


## ----label=C15rastriginsann, echo=TRUE-----------------------------------
myseed <- 1234
set.seed(myseed)
asann1 <- optim(par=start1, fn = Rastrigin, method='SANN', control=list(trace=1))
print(asann1)


## ----label=C15rastrigingensa, echo=TRUE----------------------------------
suppressPackageStartupMessages(require(GenSA))
global.min <- 0
tol <- 1e-13
set.seed(myseed)
ctrl<-list(threshold.stop=global.min+tol, verbose=TRUE)
aGenSA1 <- GenSA(lower = lower, upper = upper, fn = Rastrigin, 
           control=ctrl)
print(aGenSA1[c("value","par","counts")])


## ----label=C15rastrigindeoptim, echo=TRUE--------------------------------
suppressPackageStartupMessages(require(DEoptim))
set.seed(myseed)
ctrl <- list(trace=FALSE)
aDEopt1a <- DEoptim(lower = lower, upper = upper, fn = Rastrigin, control=ctrl)
print(aDEopt1a$optim)
tmp<-readline("Try DEoptim with more iterations")

set.seed(myseed)
ctrl <- list(itermax=10000, trace=FALSE)
aDEopt1b <- DEoptim(lower = lower, upper = upper, fn = Rastrigin, control=ctrl)
print(aDEopt1b$optim)


## ----label=C15rastriginrcppde, echo=TRUE---------------------------------
suppressPackageStartupMessages(require(RcppDE))
set.seed(myseed)
ctrl <- list(trace=FALSE)
aRcppDEopt1a <- DEoptim(lower = lower, upper = upper, fn = Rastrigin, control=ctrl)
print(aRcppDEopt1a$optim)
tmp<-readline("Try RcppDE with more iterations")

set.seed(myseed)
ctrl <- list(itermax=10000, trace=FALSE)
aRcppDEopt1b <- DEoptim(lower = lower, upper = upper, fn = Rastrigin, control=ctrl)
print(aRcppDEopt1b$optim)


## ----label=C15rastriginrcppde2, echo=TRUE--------------------------------
set.seed(123456)
ctrl <- list(itermax=10000, trace=FALSE)
aRcppDEopt1b <- DEoptim(lower = lower, upper = upper, fn = Rastrigin, control=ctrl)
print(aRcppDEopt1b$optim)


## ----label=C15rastriginsmco, echo=TRUE-----------------------------------
suppressPackageStartupMessages(require(smco))
set.seed(myseed)
asmco1 <- smco(par=rep(1,dimension), LB = lower, UB = upper, fn = Rastrigin, 
                 maxiter=10000, trc=FALSE)
print(asmco1[c("par", "value")])


## ----label=C15rastriginsoma, echo=TRUE-----------------------------------
suppressPackageStartupMessages(require(soma))
suppressPackageStartupMessages(require(reportr)) 
# NOTE: Above was not documented in soma!
setOutputLevel(OL$Warning)
set.seed(myseed)
mybounds=list(min=lower, max=upper)
myopts=list(nMigrations=100)
asoma1<-soma(Rastrigin, bounds=mybounds, options=myopts)
# print(asoma1) # Gives too much output -- not obvious to interpret.
print(asoma1$population[,asoma1$leader])
print(Rastrigin(asoma1$population[,asoma1$leader]))


## ----label=C15rastriginmalschains, echo=TRUE-----------------------------
suppressPackageStartupMessages(require(Rmalschains))
set.seed(myseed)
amals<-malschains(Rastrigin, lower=lower, upper=upper, 
         maxEvals=10000, seed=myseed, trace=FALSE)
print(amals)


## ----label=C15rastrigingenoud, echo=TRUE---------------------------------
suppressPackageStartupMessages(require(rgenoud))
set.seed(myseed)
agen1<-genoud(Rastrigin, nvars=dimension, max=FALSE, print.level=0)
print(agen1)


## ----label=C15rastriginGA, echo=TRUE-------------------------------------
suppressPackageStartupMessages(require(GA))
set.seed(myseed)
aGA1<-ga(type = "real-valued", fitness = function(x) -Rastrigin(x),
           min = lower, max = upper, popSize = 50, maxiter = 1000,
           monitor=NULL)
print(summary(aGA1))


## ----label=C15rastrigingaopt, echo=TRUE----------------------------------
suppressPackageStartupMessages(require(gaoptim))
set.seed(myseed)
minRast<-function(x) { -Rastrigin(x) } # define for minimizing
## Initial calling syntax -- no selection argument
## agaor1<-GAReal(minRast, lb=lower, ub=upper)
agaor1<-GAReal(minRast, lb=lower, ub=upper, selection='uniform')
agaor1$evolve(200) # iterate for 200 generations
## The same result was returned from 400 generations
agaor1


## ----label=C15rastrigingaopt2, echo=TRUE, fig.height=5-------------------
maxRast<-function(x){ sqrt(1/ abs(Rastrigin(x))) }
agaor2<-GAReal(maxRast, lb=lower, ub=upper, selection='fitness')
agaor2$evolve(200) 
agaor2
plot(agaor2) # note this special method for displaying results
Rastrigin(agaor2$bestIndividual())


## ----label=C15rastriginRvmmin1, echo=TRUE--------------------------------
suppressPackageStartupMessages(require(Rvmmin))
nrep<-500
bestval<-.Machine$double.xmax # start with a big number
bestpar<-rep(NA, dimension)
startmat<-matrix(NA, nrow=nrep, ncol=dimension)
set.seed(myseed)
for (i in 1:nrep) {
    for (j in 1:dimension){
        startmat[i,j]<-lower[j]+(upper[j]-lower[j])*runif(1)
    }
}
for (i in 1:nrep){
   tstart<-as.vector(startmat[i,]) 
    ans<-Rvmmin(tstart, Rastrigin, lower=lower, upper=upper)
   if (ans$value <= bestval) {
      bestval<-ans$value
      cat("Start ",i," new best =",bestval,"\n")
      bestpar<-ans$par
      bestans<-ans
   }
}
print(bestans)

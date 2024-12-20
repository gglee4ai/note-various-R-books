## ----label=C20lemoine0, echo=TRUE----------------------------------------
tspan <- c(0, 8, 13, 20, 29, 34, 41, 50, 56, 62, 
    71, 78, 83, 91, 99, 104, 111, 120, 127, 134,
    139, 148, 155, 160)
data<- c( 115, 59, 69, 142, 390, 2990, 4700, 5488, 5620, 6399, 
     5388, 4422, 3566, 2222, 1911, 2111, 1733, 1166, 831, 740, 
     780, 500, 540, 390)
## Main code for the SIR model example
require(deSolve) # Load the library to solve the ode
## initial parameter values ##
Bi <- 0.75
Bw <- 0.75
e <- 0.01
k <- 1/89193.18
## define a weight vector OUTSIDE LLode function
wts <- sqrt(sqrt(data)) 
## Combine parameters into a vector
params <- c(Bi, Bw, e*100, k*1e5)
names(params) <- c('Bi', 'Bw', 'escaled', 'kscaled')
## ODE function ##
SIRode <- function(t, x, params){
  S <- x[1]
  I <- x[2]
  W <- x[3]
  R <- x[4]
  Bi <- params[1]
  Bw <- params[2]
  e <- params[3]*1e-2
  k <- params[4]*1e-5
  dS <- -Bi*S*I - Bw*S*W
  dI <- Bi*S*I + Bw*S*W - 0.25*I
  dW <- e*(I - W)
  dR <- 0.25*I
  output <- c(dS, dI, dW, dR)
  list(output)
}
## initial conditions ##
I0 <- data[1]*k
R0 <- 0
S0 <- (1-I0)
W0 <- 0
initCond <- c(S0, I0, W0, R0)


## ----label=C20lemoine0sp, echo=TRUE, fig.width=10------------------------
## Run ODE using initial parameter guesses ##
initSim <- ode(initCond, tspan, SIRode, params, method='ode45')
plot(tspan, initSim[,3]/k, type='l', lty='dashed')
points(tspan, data)


## ----label=C20lemoinell1, echo=TRUE--------------------------------------
## Define likelihood function using weighted least squares ##
LLode <- function(params){
  k <- params[4]*1e-5
  I0 <- data[1]*k
  R0 <- 0
  S0 <- 1 - I0
  W0 <- 0
  initCond <- c(S0, I0, W0, R0)
  # Run the ODE
  odeOut <- ode(initCond, tspan, SIRode, params, method='ode45')
  if (attr(odeOut, "istate")[1] != 0) {  ## Check if satisfactory
     # 2 indicates perceived success of method 'lsoda', 0 for 'ode45'. 
     # Other integrators may have different indicator codes
     cat("Integration possibly failed\n")
     LL<-.Machine$double.xmax*1e-6 # indicate failure
  } else {
     y <- odeOut[,3]/k  # Measurement variable
     wtDiff <- (y - data)*wts  # Weighted difference
     LL <- as.numeric( crossprod(wtDiff) )  # Sum of squares
  }
  LL
}


## ----label=C20lemoineminll1, echo=TRUE-----------------------------------
## optimize using optim() ##
MLoptres <- optim(params, LLode, method='Nelder-Mead', 
      control=list(trace=0, maxit=5000))
MLoptres


## ----label=C20lemoine1sp, echo=FALSE, fig.width=10-----------------------
## run ODE using inital parameter guesses ##
mlparams<-MLoptres$par
MLSim <- ode(initCond, tspan, SIRode, mlparams, method='ode45')
plot(tspan, initSim[,3]/k, type='l', lty='dashed')
points(tspan, data)
points(tspan, MLSim[,3]/k, type='l')
title(main="Improved fit using optim:Nelder-Mead")


## ----label=C20loptjn1, echo=TRUE, eval=FALSE----------------------------- 
require(optimx)
if (! file.exists("./tmpdata/C20opresult.dput")) {
optxres <- optimx(params, LLode, lower=rep(0,4), upper=rep(500,4),
      method=c("nmkb", "bobyqa"),
      control=list(usenumDeriv=TRUE, maxit=5000, trace=0))
summary(optxres, order=value)
dput(optxres, file="./tmpdata/C20opresult.dput")
} else { 
  optxres<-dget("./tmpdata/C20opresult.dput")
}



bestpar<-coef(summary(optxres, order=value)[1,])
cat("best parameters:")
print(bestpar)
dput(bestpar, file="./tmpdata/C20bestpar.dput")

bpSim <- ode(initCond, tspan, SIRode, bestpar, method='ode45')
X11()
plot(tspan, initSim[,3]/k, type='l', lty='dashed')
points(tspan, data)
points(tspan, MLSim[,3]/k, type='l', lty='twodash')
points(tspan, bpSim[,3]/k, type='l')
title(main="Improved fit using optimx")


## ----label=C210loptjn2, echo=TRUE----------------------------------------
## from running supportdocs/ODEprobs/ODElemoine.R
optxres<-dget("./tmpdata/C20opresult.dput")
optxres
bestpar<-dget("./tmpdata/C20bestpar.dput")


## ----label=C20lemnlls1, echo=TRUE----------------------------------------
Lemres <- function(params){
  k <- params[4]*1e-5
  I0 <- data[1]*k
  R0 <- 0
  S0 <- 1 - I0
  W0 <- 0
  initCond <- c(S0, I0, W0, R0)
  nobs<-length(tspan)
  resl <- rep(NA, nobs)
  # Run the ODE
  odeOut <- ode(initCond, tspan, SIRode, params, method='ode45')
  if (attr(odeOut, "istate")[1] != 0) {  ## Check if satisfactory
     cat("Integration possibly failed\n")
     resl<-rep(.Machine$double.xmax*1e-12, nobs)
  } else {
     y <- odeOut[,3]/k
     resl <- (y - data)*wts
  }
  resl
}


## ----label=C20lemnlls2, echo=FALSE---------------------------------------
require(nlmrt)
anlfb1<-nlfb(params, Lemres, trace=FALSE, lower=rep(0,4), upper=rep(500,4) )
print(anlfb1)


## ----label=C20compres1, echo=FALSE---------------------------------------
cat("Function value LLode * 1e-8 and parameters\n")
cat("Guess,       ",LLode(params)*1e-8)
print(params)
cat("ML - Nelder, ",LLode(MLoptres$par)*1e-8)
print(MLoptres$par)
cat("Optimx-best, ",LLode(bestpar)*1e-8)
print(bestpar)
cat("nlfb-nlls,   ",LLode(coef(anlfb1))*1e-8)
print(coef(anlfb1))


# C07poissonreg2.R

source("C07poissonregdata.R", echo=FALSE) # set up data

## nonlinear least squares on residuals
ssfn <- function(beta, Y, X, obs.p){
   resfn<-U.eqn(beta, Y, X, obs.p)
   as.double(crossprod(resfn))
}
ansopt<-optim(beta, ssfn, control=list(trace=0, maxit=5000), Y=Y, X=X, obs.p=obs.p)
ansopt


## ----label=C07BBpoissonreg1c, echo=TRUE, cache=TRUE----------------------

negll <- function(beta, Y=Y, X=X, obs.p=obs.p) { # log likelihood
  Xb <- c(X %*% beta)
  ll<-crossprod(Y, Xb) - sum(obs.p*exp(Xb))
  nll <- -as.double(ll)
}
require(optimx)
print(negll(beta, Y, X, obs.p=obs.p))

ansopll<-optimx(beta, negll, gr=NULL, control=list(trace=0, usenumDeriv=TRUE, 
         all.methods=TRUE), Y=Y, X=X, obs.p=obs.p)
## Note: trace=1 gives TEX error "dimension too large".
ansopll

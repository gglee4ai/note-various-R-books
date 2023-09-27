## ----label=C12lhobbs.res, echo=TRUE--------------------------------------
lhobbs.res<-function(xl,y){ # log scaled Hobbs weeds problem -- residual
# base parameters on log(x)
    x<-exp(xl)
    if (abs(12*x[3]) > 50) { # check computability
       rbad<-rep(.Machine$double.xmax, length(x))
       return(rbad)
    }
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    t<-1:length(y)
    res<-x[1]/(1+x[2]*exp(-x[3]*t)) - y
}


## ----label=C12lhobbs.lik, echo=TRUE--------------------------------------
lhobbs.lik<-function(xaug,y=y0){ # likelihood function including sigma
  xl<-xaug[1:3]
  logSigma<-xaug[4]
  sigma2=exp(2.0*logSigma);
  res<-lhobbs.res(xl,y)
  nll<-0.5*(length(res)*log(2*pi*sigma2)+sum(res*res)/sigma2)
}


## ----label=C12lhobbs.jac, echo=TRUE--------------------------------------
lhobbs.jac<-function(xl, y) { # scaled Hobbs weeds problem -- Jacobian
    x<-exp(xl)
    jj<-matrix(0.0, 12, 3)
    t<-1:12
    yy<-exp(-x[3]*t)
    zz<-1/(1+x[2]*yy)
    jj[t,1] <- zz*exp(xl[1])
    jj[t,2] <- -x[1]*zz*zz*yy*exp(xl[2])
    jj[t,3] <- x[1]*zz*zz*yy*x[2]*t*exp(xl[3])
    return(jj)
}


## ----label=C12lhobbs.g, echo=TRUE----------------------------------------
lhobbs.g <- function(xl,y) { # scaled Hobbs weeds problem -- gradient
   shj<-lhobbs.jac(xl,y)
   shres<-lhobbs.res(xl,y)
   shg<-as.vector(2.0* (shres %*% shj))
   return(shg)
}


## ----label=C12lhobbs.lg, echo=TRUE---------------------------------------
lhobbs.lg<-function(xaug,y=y0){ # gradient function including sigma
  xl<-xaug[1:3]
  logSigma<-xaug[4]
  sigma2=exp(2.0*logSigma);
  res3<-lhobbs.res(xl,y)
  n<-length(res3)
  f3<-crossprod(res3)
  g3<-0.5*lhobbs.g(xl,y)/sigma2
  gg<-c(g3,(n - f3/sigma2))
}

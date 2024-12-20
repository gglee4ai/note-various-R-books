## ----label=C16opt2vec, echo=FALSE----------------------------------------
opt2vec <- function(method, optans) { # ignore time
## Displays optim() answers nicely
#        optans$par<-round(optans$par,6)
#        optans$value<-round(optans$value,8)
#      time<-round(time,3)      
#      outvec<-c(meth=method, fval=optans$value,
#                 par=optans$par, c=optans$counts,
#                 conv=optans$convergence, time=time)
#      return(outvec)
     cat(method,": f(",optans$par[1],", ",optans$par[2],", ",optans$par[3],")=",
        optans$value,"\n")
     cat("    after ",optans$counts[1]," fn & ",optans$counts[2]," gr evals\n")
     invisible(optans)
}
 # Note we need to repeat the code but not echo it
hobbs.f<- function(x){ # # Hobbs weeds problem -- function
    if (abs(12*x[3]) > 500) { # check computability
       fbad<-.Machine$double.xmax
       return(fbad)
    }
    res<-hobbs.res(x)
    f<-sum(res*res)
}
hobbs.res<-function(x){ # Hobbs weeds problem -- residual
# This variant uses looping
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
         38.558, 50.156, 62.948, 75.995, 91.972)
    t<-1:12
    if(abs(12*x[3])>50) {
       res<-rep(Inf,12)
    } else {
       res<-x[1]/(1+x[2]*exp(-x[3]*t)) - y
    }
}


## ----label=C16draw, echo=TRUE,fig.width=7,fig.height=4-------------------
# draw the data
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
         38.558, 50.156, 62.948, 75.995, 91.972)
    t<-1:12
    plot(t,y)
    title(main="Hobbs' weed infestation data", font.main=4)


## ----label=C16hobruns1, echo=TRUE----------------------------------------
cat("Running the Hobbs problem - code already loaded - with Nelder-Mead\n")
start <- c(100, 10, 0.1)
f0<-hobbs.f(start)
cat("initial function value=",f0,"\n")
tu<-system.time(ansnm1<-optim(start, hobbs.f, control=list(maxit=5000)))[1]
# opt2vec is a small routine to convert answers to a vector form for printing
Unscaled1<-opt2vec("unscaled Nelder",ansnm1)


## ----label=C16shobbs, echo=FALSE-----------------------------------------
shobbs.f<-function(x){ # # Scaled Hobbs weeds problem -- function
    if (abs(12*x[3]*0.1) > 50) { # check computability
       fbad<-.Machine$double.xmax
       return(fbad)
    }
    res<-shobbs.res(x)
    f<-sum(res*res)
}
shobbs.res<-function(x){ # scaled Hobbs weeds problem -- residual
# This variant uses looping
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 38.558, 50.156, 62.948,
         75.995, 91.972)
    t<-1:12
    res<-100.0*x[1]/(1+x[2]*10.*exp(-0.1*x[3]*t)) - y
}


## ----label=C16runshob, echo=TRUE-----------------------------------------
cat("Running the scaled Hobbs problem - code already loaded - with Nelder-Mead\n")
start<-c(1,1,1)
f0<-shobbs.f(start)
cat("initial function value=",f0,"\n")
ts<-system.time(ansnms1<-optim(start, shobbs.f, control=list(maxit=5000)))[1]
opt2vec("scaled Nelder",ansnms1)


## ----label=C16runhobbfgs, echo=TRUE--------------------------------------
cat("Running the Hobbs problem - code already loaded - with BFGS\n")
start<-c(100,10,.1)
starts<-c(1,1,1)
f0<-hobbs.f(start)
f0s<-shobbs.f(starts)
cat("initial function values: unscaled=",f0,"  scaled=",f0s,"\n")
tu<-system.time(abfgs1<-optim(start, hobbs.f, method='BFGS',control=list(maxit=5000)))[1]
opt2vec("BFGS unscaled", abfgs1)
ts<-system.time(abfgs1s<-optim(starts, shobbs.f, method='BFGS',control=list(maxit=5000)))[1]
opt2vec("BFGS scaled",abfgs1s)


## ----label=C16hobjac, echo=TRUE------------------------------------------
hobbs.jac<-function(x){ # Jacobian of Hobbs weeds problem
   jj<-matrix(0.0, 12, 3)
   t<-1:12
    yy<-exp(-x[3]*t)
    zz<-1.0/(1+x[2]*yy)
     jj[t,1] <- zz
     jj[t,2] <- -x[1]*zz*zz*yy
     jj[t,3] <- x[1]*zz*zz*yy*x[2]*t
   return(jj)
}

hobbs.g<-function(x){ # gradient of Hobbs weeds problem
    # NOT EFFICIENT TO CALL AGAIN
    jj<-hobbs.jac(x)
    res<-hobbs.res(x)
    gg<-as.vector(2.*t(jj) %*% res)
    return(gg)
}

shobbs.jac<-function(x) { # scaled Hobbs weeds problem -- Jacobian
    jj<-matrix(0.0, 12, 3)
    t<-1:12
    yy<-exp(-0.1*x[3]*t)
    zz<-100.0/(1+10.*x[2]*yy)
    jj[t,1] <- zz
    jj[t,2] <- -0.1*x[1]*zz*zz*yy
    jj[t,3] <- 0.01*x[1]*zz*zz*yy*x[2]*t
    return(jj)
}

shobbs.g <- function(x) { # scaled Hobbs weeds problem -- gradient
   shj<-shobbs.jac(x)
   shres<-shobbs.res(x)
   shg<-as.vector(2.0* (shres %*% shj))
   return(shg)
}

cat("Running the Hobbs problem - code already loaded - with BFGS\n")
start<-c(100,10,.1)
starts<-c(1,1,1)
f0<-hobbs.f(start)
f0s<-shobbs.f(starts)
cat("initial function values: unscaled=",f0,"  scaled=",f0s,"\n")
tu<-system.time(abfgs1g<-optim(start, hobbs.f, gr=hobbs.g, method='BFGS',control=list(maxit=5000)))[1]
opt2vec("BFGS unscaled",abfgs1g)
ts<-system.time(abfgs1sg<-optim(starts, shobbs.f, gr=shobbs.g, method='BFGS',control=list(maxit=5000)))[1]
opt2vec("BFGS scaled",abfgs1sg)


## ----label=C16parscalhob, echo=TRUE--------------------------------------
cat("Try using parscale\n")
start<-c(100,10,0.1)
anmps1<-optim(start,hobbs.f,
      control=list(trace=FALSE,parscale=c(100,10,.1) ) )
opt2vec("Nelder + parscale",anmps1)
abfps1<-optim(start,hobbs.f,gr=hobbs.g, method='BFGS', 
      control=list(trace=TRUE,parscale=c(100,10,.1) ) )
opt2vec("BFGS + parscale",abfps1)


## ----label=C16kkthob, echo=FALSE-----------------------------------------
source("kktcheck.R") # Modified for website to include in C16 directory
cat("results from KKT check of abfgs1sg solution (scaled, with gradient from BFGS)\n")
kbfgs1sg<-kktcheck(abfgs1sg,shobbs.f,shobbs.g)


## ----label=C16kkthob2, echo=FALSE----------------------------------------
# source("supportdocs/scaling/kktcheck.R")
cat("results from KKT check of abfps1 solution (parscaled, with gradient from BFGS)\n")
kbfps1<-kktcheck(abfps1,hobbs.f,hobbs.g)


## ----label=C16kkthob3, echo=FALSE----------------------------------------
# source("supportdocs/scaling/kktcheck.R")
cat("results from KKT check of abfgs1g solution (unscaled, with gradient from BFGS)\n")
kbfgs1g<-kktcheck(abfgs1g,hobbs.f,hobbs.g)

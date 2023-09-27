## ----label=C12run3par, echo=TRUE-----------------------------------------
y0 <- c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
        38.558, 50.156, 62.948, 75.995, 91.972)
source("C12lhobbs.R", echo=FALSE) # to supply the function and gradient
## Above is same as C11lhobbs.R -- here for self-standing chapter directory
require(Rvmmin)
start<-c(1,1,1,1)
bdmsk<-c(1,1,1,0) # Cat fix parameter 4 at first
## afix4<-Rvmmintry(start, lhobbs.lik, lhobbs.lg, bdmsk=bdmsk, y=y0,
##        control=list(trace=2))
afix4<-Rvmmin(start, lhobbs.lik, lhobbs.lg, bdmsk=bdmsk, y=y0,
       control=list(trace=0))
print(afix4)


## ----label=C12run4parafter3, echo=TRUE-----------------------------------
start2<-afix4$par
## new start
print(start2)
bdmsk<-rep(1,4) ## EXPLICITLY free all parameters
aall<-Rvmmin(start2,lhobbs.lik, lhobbs.lg, bdmsk=bdmsk, y=y0)
print(aall)


## ----label=C12run4fromguess, echo=FALSE----------------------------------
print(start)
aall0<-Rvmmin(start,lhobbs.lik, lhobbs.lg, y=y0)
print(aall0)
maskcounts<-afix4$counts+aall$counts
freecounts<-aall0$counts
cat("\n\nComparison of work: Masked      vs.    Free \n")
cat("    Functions         ",maskcounts[1],"           ",freecounts[1],"\n")
cat("    Gradients          ",maskcounts[2],"            ",freecounts[2],"\n")
cat("\n Exponentiated parameters, last is sigma:")
print(exp(aall$par))


## ----label=C12bateshobbsmask1, echo=TRUE---------------------------------
weeddata<-data.frame(y=y0, t=1:12)
mystart<-c(Asym=250, xmid=6, scal=1) # This sets the Asym value
require(nlmrt) # Ensure tools available
maskrun<-nlxb(y~Asym/(1+exp((xmid-t)/scal)), start=mystart, data=weeddata, 
      mask=c("Asym"), trace=FALSE)
maskrun


## ----label=C12bateshobbsmask2, echo=TRUE---------------------------------
mystart2<-c(xmid=6, scal=1) # This sets the Asym value
maskrun2<-nlxb(y~250/(1+exp((xmid-t)/scal)), start=mystart2, data=weeddata)
maskrun2

## ----label=C16onepercent1, echo=FALSE------------------------------------

## Following included so script is self-standing
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

hobbs.f<- function(x){ # # Hobbs weeds problem -- function
    cat("structure of x\n")
    print(str(x))
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




## Bates model: y = Asym/(1+exp((xmid-t)/scal))
##      = b1 / (1 + exp(xmid/scal)*exp(-t/scal))
##      = b1 / (1 + b2 * exp(-b3*t))
##  where b2 = exp(xmid/scal)
##        b3 = 1/scal        b1 = Asym
##    So Asym = b1,  scal = 1/b3, xmid = log(b2) * scal = log(b2)/b3

hbates.res<-function(x) { 
    if(length(x) != 3) stop("hobbs.res -- parameter vector n!=3")
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
         38.558, 50.156, 62.948, 75.995, 91.972)
    t<-1:12
#    if(abs(12*x[3])>50) {
#       res<-rep(Inf,12)
#    } else {
       res<-x[1]/(1+exp((x[2]-t)/x[3])) - y
#    }
}

hbates.f<- function(x){ ## Hobbs weeds problem -- function
    res<-hbates.res(x)
    f<-sum(res*res)
}

hbates.jac<-function(x){ # Jacobian of Hobbs weeds problem
   jj<-matrix(0.0, 12, 3)
   t<-1:12
    yy<-exp((x[2]-t)/x[3])
    zz<-1.0/(1+yy)
     jj[ ,1] <- zz
     jj[ ,2] <- -x[1]*zz*zz*yy/x[3]
     jj[ ,3] <- x[1]*zz*zz*yy*(x[2]-t)/(x[3]*x[3])
   return(jj)
}

hbates.g<-function(x){ # gradient of Hobbs weeds problem
    # NOT EFFICIENT TO CALL AGAIN
    jj<-hbates.jac(x)
    res<-hbates.res(x)
    gg<-as.vector(2.*t(jj) %*% res)
    return(gg)
}


options(width=100)
## cat("Running the scaled Hobbs problem - code already loaded - with Nelder-Mead\n")
s0s<-c(1,1,1)
sf0<-shobbs.f(s0s)
s0s1<-s0s*c(1.01,1,1)
s0s2<-s0s*c(1,1.01,1)
s0s3<-s0s*c(1,1,1.01)
sf01<-shobbs.f(s0s1)
sf02<-shobbs.f(s0s2)
sf03<-shobbs.f(s0s3)
sshift0<-c(sf0, 100*(sf01-sf0)/sf0, 100*(sf02-sf0)/sf0, 100*(sf03-sf0)/sf0)
## cat("shobbs.f -- Initial function at c(1,1,1) and % shift for 1% change in each parameter\n")
## print(sshift0)
ts<-system.time(ansnms1<-optim(s0s, shobbs.f, control=list(maxit=5000)))[1]
sparx<-ansnms1$par
sparx1<-sparx*c(1.01, 1, 1)
sparx2<-sparx*c(1, 1.01, 1)
sparx3<-sparx*c(1, 1, 1.01)
sfx<-shobbs.f(sparx)
sfx1<-shobbs.f(sparx1)
sfx2<-shobbs.f(sparx2)
sfx3<-shobbs.f(sparx3)
sshiftx<-c(sfx, 100*(sfx1-sfx)/sfx, 100*(sfx2-sfx)/sfx, 100*(sfx3-sfx)/sfx)
## cat("shobbs.f -- Final function at c(1,1,1) and % shift for 1% change in each parameter\n")
## print(sshiftx)
u0s<-c(100, 10, 0.1)
uf0<-hobbs.f(u0s)
u0s1<-c(1.01,1,1)*u0s
u0s2<-c(1,1.01,1)*u0s
u0s3<-c(1,1,1.01)*u0s
uf01<-hobbs.f(u0s1)
uf02<-hobbs.f(u0s2)
uf03<-hobbs.f(u0s3)
ushift0<-c(uf0, 100*(uf01-uf0)/uf0, 100*(uf02-uf0)/uf0, 100*(uf03-uf0)/uf0)
## cat("hobbs.f -- Initial function at c(1,1,1) and % shift for 1% change in each parameter\n")
## print(ushift0)
uparx<-sparx*c(100, 10, 0.1)
uparx1<-uparx*c(1.01, 1, 1)
uparx2<-uparx*c(1, 1.01, 1)
uparx3<-uparx*c(1, 1, 1.01)
ufx<-hobbs.f(uparx)
ufx1<-hobbs.f(uparx1)
ufx2<-hobbs.f(uparx2)
ufx3<-hobbs.f(uparx3)
ushiftx<-c(ufx, 100*(ufx1-ufx)/ufx, 100*(ufx2-ufx)/ufx, 100*(ufx3-ufx)/ufx)
## cat("hobbs.f -- Final function at c(1,1,1) and % shift for 1% change in each parameter\n")
##print(ushiftx)


b1<-u0s[1]
b2<-u0s[2]
b3<-u0s[3]
b0s<-c(b1,log(b2)/b3,1/b3)
b0s1<-c(1.01,1,1)*b0s
b0s2<-c(1,1.01,1)*b0s
b0s3<-c(1,1,1.01)*b0s
bf0<-hbates.f(b0s)
bf01<-hbates.f(b0s1)
bf02<-hbates.f(b0s2)
bf03<-hbates.f(b0s3)
bshift0<-c(bf0, 100*(bf01-bf0)/bf0, 100*(bf02-bf0)/bf0, 100*(bf03-bf0)/bf0)
## cat("hbates.f -- Initial function at c(1,1,1) and % shift for 1% change in each parameter\n")
## print(bshift0)

b1<-uparx[1]
b2<-uparx[2]
b3<-uparx[3]
bparx<-c(b1,log(b2)/b3,1/b3)
bparx1<-c(1.01,1,1)*bparx
bparx2<-c(1,1.01,1)*bparx
bparx3<-c(1,1,1.01)*bparx
bfx<-hbates.f(bparx)
bfx1<-hbates.f(bparx1)
bfx2<-hbates.f(bparx2)
bfx3<-hbates.f(bparx3)
bshiftx<-c(bfx, 100*(bfx1-bfx)/bfx, 100*(bfx2-bfx)/bfx, 100*(bfx3-bfx)/bfx)
## cat("hbates.f -- Final function at c(1,1,1) and % shift for 1% change in each parameter\n")
## print(bshiftx)

scaltable<-rbind(sshift0, ushift0, bshift0, sshiftx, ushiftx, bshiftx)
rownames(scaltable)<-c("shobbs - start", "uhobbs - start", "hbates - start",
      "shobbs - final", "uhobbs - final", "hbates - final")
colnames(scaltable)<-c("fn value","1% p1 effect","1% p2 effect", "1% p3 effect")
print(scaltable)


## ----label=C16genran1000s, echo=TRUE, cache=FALSE------------------------
# generate 1000 sets of points in [0, 5) with runif()
set.seed(12345)
nrun<-1000
sstart<-matrix(runif(3*nrun, 0, 5), nrow=nrun, ncol=3)
ustart<-sstart %*% diag(c(100, 10, 0.1))
bstart<-matrix(NA, nrow=nrun, ncol=3)
for (i in 1:nrun) {
  b1<-ustart[[i,1]]
  b2<-ustart[[i,2]]
  b3<-ustart[[i,3]]
  Asym<-b1
  scal<-1/b3
  xmid<-log(b2)/b3
  bstart[[i,1]]<-Asym
  bstart[[i,2]]<-xmid
  bstart[[i,3]]<-scal
}


## ----label=C16ranbates1, echo=TRUE---------------------------------------
## random Bates start
# generate 1000 sets of points in [0, 5) with runif()
set.seed(12345)
nrun<-1000
bstart<-matrix(runif(3*nrun, 0, 1), nrow=nrun, ncol=3)
minasymp<-92
bstart[,1]<-bstart[,1]*(500-minasymp) + minasymp
bstart[,2]<-bstart[,2]*9 + 2 # inside t interval
scal0<-0.05
bstart[,3]<-bstart[,3]*(5-scal0)+scal0 # No check for zero!
sstart<-matrix(NA, nrow=nrun, ncol=3)
ustart<-sstart
for (i in 1:nrun) {
  Asym<-bstart[[i,1]]
  xmid<-bstart[[i,2]]
  scal<-bstart[[i,3]]
  b1<-Asym
  b2<-exp(xmid/scal)
  b3<-1/scal
  ustart[[i,1]]<-b1
  sstart[[i,1]]<-b1/100
  ustart[[i,2]]<-b2
  sstart[[i,2]]<-b2/10
  ustart[[i,3]]<-b3
  sstart[[i,3]]<-b3*10
}
cat("output Bates starts\n")
cat("./tmpdata/batestrtb.dput\n")
dput(bstart, file="./tmpdata/batestrtb.dput")
cat("./tmpdata/batestrts.dput\n")
dput(sstart, file="./tmpdata/batestrts.dput")
cat("./tmpdata/batestrtu.dput\n")
dput(ustart, file="./tmpdata/batestrtu.dput")


## ----label=C16chkstart, echo=TRUE----------------------------------------

exlist<-c() # empty
extype<-c() # to record type
nex=0

cat("Run scaled model -- ")
for (i in 1:nrun) {
#for (i in 150:160) {
   st<-as.numeric(bstart[i,])
   ga<-hbates.g(st)
   badgrad<-FALSE
   if (any(is.na(ga)) | any(! is.finite(ga))) badgrad<-TRUE
   if (badgrad) {
     exlist<-c(exlist,i)
     extype<-c(extype,"B")
   }
}
cat("found ",length(exlist)-nex," bad starts\n")
nex<-length(exlist)

cat("Run scaled model -- ")
for (i in 1:nrun) {
# for (i in 150:160) {
   st<-as.numeric(sstart[i,])
   ga<-shobbs.g(st)
   badgrad<-FALSE
   if (any(is.na(ga)) | any(! is.finite(ga))) badgrad<-TRUE
   if (badgrad) {
   exlist<-c(exlist,i)
   extype<-c(extype,"S")
   }
}
cat("found ",length(exlist)-nex," bad starts\n")
nex<-length(exlist)

cat("Run unscaled model -- ")
for (i in 1:nrun) {
# for (i in 150:160) {
   st<-as.numeric(ustart[i,])
   ga<-hobbs.g(st)
   badgrad<-FALSE
   if (any(is.na(ga)) | any(! is.finite(ga))) badgrad<-TRUE
   if (badgrad) {
   exlist<-c(exlist,i)
   extype<-c(extype,"U")
   }
}
cat("found ",length(exlist)-nex," bad starts\n")
nex<-length(exlist)
exlist<-unique(exlist)
options(width=80)
print(exlist)


## ----label=C16robust1000, echo=FALSE-------------------------------------
nrun<-1000
require(optimx)
# assumes we are working in nlpor directory
cat("Current directory is ",getwd(),"\n")
if (! file.exists("./tmpdata/brunb.dput")){
  bstart<-dget("./tmpdata/batestrtb.dput")
  ## Run bates model
  ifirst<-0 # no structure for first starting parameters
  for (i in 1:nrun) {
     if (! (i %in% exlist)){
       st<-as.numeric(bstart[i,])
       ans<-optimx(st, hbates.f, hbates.g, method="all")
       ifirst<-ifirst+1
       ans$start<-i
       ans$model<-"hbates"
       ans$meth<-rownames(ans)
       if (i == ifirst) {brunb<-ans}
       else {brunb<-rbind(brunb, ans)}
     }
  }
  rownames(brunb)<-NULL
  dput(brunb, file="./tmpdata/brunb.dput")
} else {
   brunb<-dget("./tmpdata/brunb.dput")
}

if (! file.exists("./tmpdata/bruns.dput")){
   sstart<-dget("./tmpdata/batestrts.dput")
   ## Run scaled model
   ifirst<-0 # no structure for first starting parameters
   for (i in 1:nrun) {
      if (! (i %in% exlist)){
         st<-as.numeric(sstart[i,])
         ans<-optimx(st, shobbs.f, shobbs.g, method="all")
         ifirst<-ifirst+1
         ans$start<-i
         ans$model<-"shobbs"
         ans$meth<-rownames(ans)
         if (i == ifirst) {bruns<-ans}
         else {bruns<-rbind(bruns, ans)}
      }
   }
   rownames(bruns)<-NULL
   dput(bruns, file="./tmpdata/bruns.dput")
} else {
   bruns<-dget("./tmpdata/bruns.dput")
}

if (! file.exists("./tmpdata/brunu.dput")) {
   ustart<-dget("./tmpdata/batestrtu.dput")
   ## Run unscaled model
   ifirst<-0 # no structure for first starting parameters
   for (i in 1:nrun) {
      if (! (i %in% exlist)){
         st<-as.numeric(ustart[i,])
         ans<-optimx(st, hobbs.f, hobbs.g, method="all")
         ifirst<-ifirst+1
         ans$start<-i
         ans$model<-"uhobbs"
         ans$meth<-rownames(ans)
         if (i == ifirst) {brunu<-ans}
         else {brunu<-rbind(brunu, ans)}
      }
   }
   rownames(brunu)<-NULL
   dput(brunu, file="./tmpdata/brunu.dput")
} else {
   brunu<-dget("./tmpdata/brunu.dput")
}
## Correct parameter values and names to unscaled form
bruns$b1<-100*bruns$p1
bruns$b2<-10*bruns$p2
bruns$b3<-0.1*bruns$p3
brunb$b1<-brunb$p1
brunb$b2<-exp(brunb$p2/brunb$p3)
brunb$b3<-1/brunb$p3
# and null out some values so we can put dataframes together
brunu$b1<-brunu$p1
brunu$b2<-brunu$p2
brunu$b3<-brunu$p3
brunb$Asym<-NULL
brunb$xmid<-NULL
brunb$scal<-NULL
bruns$p1<-NULL
bruns$p2<-NULL
bruns$p3<-NULL
brunu$p1<-NULL
brunu$p2<-NULL
brunu$p3<-NULL
brunb$p1<-NULL
brunb$p2<-NULL
brunb$p3<-NULL
brunu$Asym<-NULL
brunu$xmid<-NULL
brunu$scal<-NULL
bruns$Asym<-NULL
bruns$xmid<-NULL
bruns$scal<-NULL
# put all runs together
brunall<-rbind(bruns, brunu, brunb)
brunall$lt2.6 <- (brunall$value < 2.6)
cat("Result has function value < 2.6\n")
mybrun<-table(brunall$lt2.6, brunall$model, useNA="ifany")
mybrun<-addmargins(mybrun)
rownames(mybrun)<-c("ss>=2.6","ss <2.6","Total")
colnames(mybrun)<-c(colnames(mybrun)[1:3],"Total")
print(mybrun)


## ----label=C16robust1000k, echo=TRUE-------------------------------------
brunall$lt2.6k <- (brunall$lt2.6 & brunall$kkt1 & brunall$kkt2)
myopt26k<-table(brunall$lt2.6k, brunall$model, useNA="no")
print(myopt26k)
##
## Get the first unscaled solution parameters
hobbu1<-as.numeric(c(brunu[1,"b1"],brunu[1,"b2"],brunu[1,"b3"] ))
cat("First unscaled solution has sum of squares=",hobbs.f(hobbu1),"\n")
require(numDeriv)
hessu1<-hessian(hobbs.f, hobbu1)
## Hessian of the unscaled function at this solution
print(hessu1)
## Eigenvalues
print(eigen(hessu1)$value)
## Gradient of unscaled function
print(as.numeric(hobbs.g(hobbu1)))
hobbu1s<-hobbu1*c(0.01, 0.1, 10) # scale the parameters
## Hessian of the scaled function at this solution (when scaled)
shessu1<-hessian(shobbs.f, hobbu1s)
print(shessu1)
## Eigenvalues
print(eigen(shessu1)$value)
## Gradient of scaled function
print(as.numeric(shobbs.g(hobbu1s)))


## ----label=C16robust1000a, echo=FALSE------------------------------------
brunall$good<-brunall$lt2.6
brunall$good[which(! brunall$good)]<-NA
mytable<-table(brunall$good, brunall$meth, brunall$model)
rownames(mytable)<-""
mytab2<-addmargins(mytable, quiet=TRUE)[1,,]
ftable(mytab2)


## ----label=C16robhobnl1, echo=FALSE--------------------------------------
y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
       38.558, 50.156, 62.948, 75.995, 91.972)
t<-1:12
weeddata<-data.frame(y=y, t=t)
nrun<-1000
require(nlmrt)
require(minpack.lm)
# assumes we are working in nlpor directory
formulau<-y ~ b1/(1+b2*exp(-b3*t))
formulas<-y ~ 100*sb1/(1+10*sb2*exp(-0.1*sb3*t))
formulab<-y ~ Asym/(1+exp((xmid-t)/scal))
bnames<-c("Asym", "xmid", "scal")
snames<-c("sb1", "sb2", "sb3")
unames<-c("b1", "b2", "b3")
badnls<-list(b1=NA, b2=NA, b3=NA, value=1e+300, conv=9999)
badnlxb<-list(b1=NA, b2=NA, b3=NA, value=1e+300, conv=9999)
badnlsLM<-list(b1=NA, b2=NA, b3=NA, value=1e+300, conv=9999)

if (! file.exists("./tmpdata/nlrunb.dput")){
  bstart<-dget("./tmpdata/batestrtb.dput")
  ## Run bates model
  ifirst<-0 # no structure for first starting parameters
  for (i in 1:nrun) {
     if (! (i %in% exlist)){
       ifirst<-ifirst+1
       st<-as.numeric(bstart[i,])
       names(st)<-bnames
       ans<-try(nls(formulab, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anls<-badnls }
       else { tcoef<-coef(ans)
              anls<-list(b1=tcoef[1], b2=exp(tcoef[2]/tcoef[3]),
                     b3=1/tcoef[3], value=hbates.f(tcoef), conv=0)
             }
       anls$start<-i
       anls$model<-"hbates"
       anls$meth<-"nls"
       if (ifirst == 1) {
          nlrunb<-anls
       } else {
          nlrunb<-rbind(nlrunb, anls)
       }
       ## nlxb
       ans<-try(nlxb(formulab, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlxb<-badnls }
       else { tcoef<-coef(ans)
              anlxb<-list(b1=tcoef[1], b2=exp(tcoef[2]/tcoef[3]),
                     b3=1/tcoef[3], value=hbates.f(tcoef), conv=0)
       }
       anlxb$start<-i
       anlxb$model<-"hbates"
       anlxb$meth<-"nlxb"
       nlrunb<-rbind(nlrunb, anlxb)
       ## nlsLM
       ans<-try(nlsLM(formulab, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlsLM<-badnls }
       else { tcoef<-coef(ans)
              anlsLM<-list(b1=tcoef[1], b2=exp(tcoef[2]/tcoef[3]),
                     b3=1/tcoef[3], value=hbates.f(tcoef), conv=0)
       }
       anlsLM$start<-i
       anlsLM$model<-"hbates"
       anlsLM$meth<-"nlsLM"
       nlrunb<-rbind(nlrunb, anlsLM)
     }
     cat(i," ")
  }
  rownames(nlrunb)<-NULL
  dput(nlrunb, file="./tmpdata/nlrunb.dput")
} else {
   nlrunb<-dget("./tmpdata/nlrunb.dput")
}

if (! file.exists("./tmpdata/nlruns.dput")){
  sstart<-dget("./tmpdata/batestrts.dput")
  ## Run scaled model
  ifirst<-0 # no structure for first starting parameters
  for (i in 1:nrun) {
     if (! (i %in% exlist)){
       ifirst<-ifirst+1
       st<-as.numeric(sstart[i,])
       names(st)<-snames
       ans<-try(nls(formulas, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anls<-badnls }
       else { tcoef<-coef(ans)
              anls<-list(b1=100*tcoef[1], b2=10*tcoef[2],
                     b3=0.1*tcoef[3], value=shobbs.f(tcoef), conv=0)
             }
       anls$start<-i
       anls$model<-"shobbs"
       anls$meth<-"nls"
       if (ifirst == 1) {
          nlruns<-anls
       } else {
          nlruns<-rbind(nlruns, anls)
       }
       ## nlxb
       ans<-try(nlxb(formulas, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlxb<-badnls }
       else { tcoef<-coef(ans)
              anlxb<-list(b1=100*tcoef[1], b2=10*tcoef[2],
                     b3=0.1*tcoef[3], value=shobbs.f(tcoef), conv=0)
       }
       anlxb$start<-i
       anlxb$model<-"shobbs"
       anlxb$meth<-"nlxb"
       nlruns<-rbind(nlruns, anlxb)
       ## nlsLM
       ans<-try(nlsLM(formulas, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlsLM<-badnls }
       else { tcoef<-coef(ans)
              anlsLM<-list(b1=100*tcoef[1], b2=10*tcoef[2],
                     b3=0.1*tcoef[3], value=shobbs.f(tcoef), conv=0)
       }
       anlsLM$start<-i
       anlsLM$model<-"shobbs"
       anlsLM$meth<-"nlsLM"
       nlruns<-rbind(nlruns, anlsLM)
     }
     cat(i," ")
  }
  rownames(nlruns)<-NULL
  dput(nlruns, file="./tmpdata/nlruns.dput")
} else {
   nlruns<-dget("./tmpdata/nlruns.dput")
}


if (! file.exists("./tmpdata/nlrunu.dput")){
  ustart<-dget("./tmpdata/batestrtu.dput")
  ## Run unscaled model
  ifirst<-0 # no structure for first starting parameters
  for (i in 1:nrun) {
     if (! (i %in% exlist)){
       ifirst<-ifirst+1
       st<-as.numeric(ustart[i,])
       names(st)<-unames
       ans<-try(nls(formulau, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anls<-badnls }
       else { tcoef<-coef(ans)
              anls<-list(b1=tcoef[1], b2=tcoef[2],
                     b3=tcoef[3], value=hobbs.f(tcoef), conv=0)
             }
       anls$start<-i
       anls$model<-"uhobbs"
       anls$meth<-"nls"
       if (ifirst == 1) {
          nlrunu<-anls
       } else {
          nlrunu<-rbind(nlrunu, anls)
       }
       ## nlxb
       ans<-try(nlxb(formulau, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlxb<-badnls }
       else { tcoef<-coef(ans)
              anlxb<-list(b1=tcoef[1], b2=tcoef[2],
                     b3=tcoef[3], value=hobbs.f(tcoef), conv=0)
       }
       anlxb$start<-i
       anlxb$model<-"uhobbs"
       anlxb$meth<-"nlxb"
       nlrunu<-rbind(nlrunu, anlxb)
       ## nlsLM
       ans<-try(nlsLM(formulau, start=st, data=weeddata), silent=TRUE)
       if (class(ans) == "try-error") { anlsLM<-badnls }
       else { tcoef<-coef(ans)
              anlsLM<-list(b1=tcoef[1], b2=tcoef[2],
                     b3=tcoef[3], value=hobbs.f(tcoef), conv=0)
       }
       anlsLM$start<-i
       anlsLM$model<-"uhobbs"
       anlsLM$meth<-"nlsLM"
       nlrunu<-rbind(nlrunu, anlsLM)
     }
     cat(i," ")
  }
  rownames(nlrunu)<-NULL
  dput(nlrunu, file="./tmpdata/nlrunu.dput")
} else {
   nlrunu<-dget("./tmpdata/nlrunu.dput")
}


## ----label=C16robhobnl2, echo=TRUE---------------------------------------
## save computation by reading data
 nlruns<-dget(file="./tmpdata/nlruns.dput")
 nlrunu<-dget(file="./tmpdata/nlrunu.dput")
 nlrunb<-dget(file="./tmpdata/nlrunb.dput")
nlrunall <- rbind( nlruns, nlrunu, nlrunb)
nlrunall <- as.data.frame(nlrunall)
nlrunall[,"meth"]<-as.character(nlrunall[,"meth"])
nlrunall[,"model"]<-as.character(nlrunall[,"model"])
nlrunall<-as.data.frame(nlrunall)
rownames(nlrunall)<-NULL
nlrunall$value[which(is.na(nlrunall$value))]<-1e300
nlrunall$lt2.6<-FALSE
nlrunall$lt2.6[which(nlrunall$value<2.6)]<-TRUE
## Successes by models and methods
mynltab<-table(nlrunall$lt2.6, nlrunall$meth, nlrunall$model)[2,,]
mynltab<-addmargins(mynltab)
rownames(mynltab)<-c(rownames(mynltab)[1:3],"Total")
colnames(mynltab)<-c(colnames(mynltab)[1:3],"Total")
print(mynltab)


## ----label=C16hbnlxbtest1, echo=TRUE-------------------------------------
which(nlrunall$start==67)
test<-nlrunall[195,]
print(test)
tpar<-c(test["b1"], test["b2"], test["b3"])
tpar<-unlist(tpar) # this seems to be necessary
spar<-tpar*c(0.01, 0.1, 10) # scaled parameters
names(spar)<-snames
## scaled parameters
ftest<-shobbs.f(spar)
ftest
gg<-shobbs.g(spar)
gg
require(numDeriv)
## We use jacobian of gradient rather than hessian of function
hh<-jacobian(shobbs.g, spar)
hh
eigen(hh)$values
## ... BUT ...
utest<-hobbs.f(tpar)
utest
gu<-hobbs.g(tpar)
gu
hu<-jacobian(hobbs.g, tpar)
eigen(hu)$values

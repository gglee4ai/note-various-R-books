## ----label=C14runge1, echo=TRUE, cache=TRUE------------------------------

# functions included so the script is self-standing
sumabs<-function(x, A, y){ # loss function
   sum(abs(y-A%*%x))
}
sumsqr<-function(x, A, y){ # loss function
   sum((y-A%*%x)^2) # a poor way to do this
}

minmaxw<-function(x, A, y){
   res<-y-A%*%x
   val<-max(abs(res))
   attr(val,"where")<-which(abs(res)==val)
   val
}
minmax<-function(x, A, y){
   res<-y-A%*%x
   val<-max(abs(res))
}
## ----label=C14mplreg3, echo=TRUE, cache=TRUE-----------------------------
LADreg <- function(A, y){ # Least sum abs residuals regression
   require(linprog)
   m<-dim(A)[1]
   n<-dim(A)[2]
   if (length(y) != m) stop("Incompatible A and y sizes in LADreg")
   ones<-diag(rep(1,m))
   Abig<-rbind(cbind(-ones, -A,A),cbind(-ones, A,-A))
   bvec<-c(-y, y)
   cvec<-c(rep(1,m),rep(0,2*n)) # m z's, n pos x, n neg x
   ans<-solveLP(cvec, bvec, Abig, lpSolve=TRUE)
   sol<-ans$solution
   coeffs<-sol[(m+1):(m+n)]-sol[(m+n+1):(m+2*n)]
   names(coeffs)<-as.character(1:n)
   res<-as.numeric(sol[1:m])
   sad<-sum(abs(res))
   answer<-list(coeffs=coeffs, res=res, sad=sad)
}

## ----label=C14mmreg2, echo=TRUE, cache=TRUE------------------------------
mmreg <- function(A, y){ # max abs residuals regression
   require(linprog)
   m<-dim(A)[1]
   n<-dim(A)[2]
   if (length(y) != m) stop("Incompatible A and y sizes in LADreg")
   onem<-rep(1,m)
   Abig<-rbind(cbind(-A,A,-onem),cbind(A,-A, -onem))
   bvec<-c(-y, y)
   cvec<-c(rep(0,2*n),1) # n pos x, n neg x, 1 t
   ans<-solveLP(cvec, bvec, Abig, lpSolve=TRUE)
   sol<-ans$solution
   coeffs<-sol[1:n]-sol[(n+1):(2*n)]
   names(coeffs)<-as.character(1:n)
   res<-as.numeric(A%*%coeffs - y)
   mad<-max(abs(res))
   answer<-list(coeffs=coeffs, res=res, mad=mad)
}

###


n <- 10                             # polynomial of degree 10
m <- 101                            # no. of data points
xi <- seq(-1, 1, length = m)
yi <- 1 / (1 + (5*xi)^2)
plot(xi, yi, type='l', lwd=3, col="gray", main="Runge's Function")
title(sub="Polynomial approximation is dot-dash line")

pfn <- function(p) max(abs(polyval(c(p), xi) - yi))
   
require(pracma)
pf <- polyfit(xi, yi, n)
print(pfn(pf))

lines(xi, polyval(pf, xi), col="blue", lty=4)


## ----label=C14runge2, echo=TRUE, cache=TRUE------------------------------
## set up a matrix for the polynomial data
ndeg<-10
Ar <- matrix(NA, nrow=101, ncol=ndeg+1) 
Ar[,ndeg+1]<-1
for (j in 1:ndeg){ # align to match polyval
   Ar[,ndeg+1-j]<-xi*Ar[,ndeg+2-j]
}
cat("Check minimax function value for least squares fit\n")
print(minmaxw(pf, Ar, yi))

apoly1<-mmreg(Ar, yi)
apoly1$mad
pcoef<-apoly1$coeffs
pcoef
sumabs(pcoef, Ar, yi)
sumsqr(pcoef, Ar, yi)
print(minmaxw(pcoef, Ar, yi))


## ----label=C14runge3, echo=TRUE, cache=TRUE------------------------------
## LAD regression on Runge
apoly2<-LADreg(Ar, yi)
apoly2$sad
pcoef2<-apoly2$coeffs
pcoef2
sumabs(pcoef2, Ar, yi)
sumsqr(pcoef2, Ar, yi)
print(minmaxw(pcoef2, Ar, yi))


## ----label=C14runge4, echo=TRUE, cache=TRUE------------------------------
require(adagio)
system.time(aadag<-pureCMAES(pf, pfn, rep(-200, 11), rep(200,11)))
aadag

## ----label=C14mplreg1, echo=TRUE, cache=TRUE-----------------------------
x<-c(3,-2,-1)
a1<-rep(1,10)
a2<-1:10
a3<-a2^2
A<-matrix(c(a1, a2, a3), nrow=10, ncol=3)
A
## create model data
y0=A%*%x
y0
## perturb the model data -- get working data
set.seed(1245)
y<-as.numeric(y0)+runif(10,-1,1)
y
## try a least squares model
l2fit<-lm(y~a2+a3)
l2fit


## ----label=C14mplreg2, echo=TRUE, cache=TRUE-----------------------------
sumabs<-function(x, A, y){ # loss function
   sum(abs(y-A%*%x))
}
sumsqr<-function(x, A, y){ # loss function
   sum((y-A%*%x)^2) # a poor way to do this
}
require(optimx)
st<-rep(1,3)
ansL1<-optimx(st, sumabs, method="all", control=list(usenumDeriv=TRUE), A=A, y=y)
print(summary(ansL1, order=value))
ansL2<-optimx(st, sumsqr, method="all", control=list(usenumDeriv=TRUE), A=A, y=y)
print(summary(ansL2, order=value))
## l2fit sum of squares
sumsqr(coef(l2fit), A, y)


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


## ----label=C14mplreg4, echo=TRUE, cache=TRUE-----------------------------
myans1<-LADreg(A,y)
myans1$coeffs
myans1$sad


## ----label=C14mplreg5, echo=TRUE, cache=TRUE-----------------------------
require(pracma)
ap1<-L1linreg(A=A, b=y)
ap1
sumabs(ap1$x, A, y)
require(quantreg)
qreg<-rq(y ~ a2+a3, tau=0.5)
qreg
xq<-coef(qreg)
sumabs(xq,A,y)

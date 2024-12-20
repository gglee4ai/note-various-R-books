## ----label=C14mmreg1, echo=TRUE------------------------------------------
# data generation so script is self-standing
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
# end data generation
# other functions needed for self-standing script
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
require(optimx)
# ??
require(optextras)
st<-c(1,1,1)
ansmm1<-optimx(st, minmax, gr="grnd", method="all", A=A, y=y)
print(summary(ansmm1, order=value))


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


## ----label=C14mmreg3, echo=TRUE, cache=TRUE------------------------------
amm<-mmreg(A,y)
amm
mmcoef<-amm$coeffs
## sum of squares of minimax solution
print(sumsqr(mmcoef, A, y))
## sum abs residuals of minimax solution
print(sumabs(mmcoef, A, y))

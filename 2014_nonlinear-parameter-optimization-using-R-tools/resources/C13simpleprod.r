## ----label=C13simpleproduct, echo=TRUE, eval=FALSE-----------------------
## require(optimx, quietly=TRUE)
## # load a range of optimization methods
## pr <- function(y) {
## - prod(y)*(1-sum(y))
## }
## pr.g <- function(x) {
## g<- -prod(x)*(1-sum(x))/x + prod(x)
## }
## n<-2
## cat("  n","\t","  x1","\t\t\t"," 1e5*(x1-1/n)\n")
## options(digits=6)
## while (n <= 6) {
##    st<-1:(n-1)/(n*n)
##    ctrl<-list(starttests=FALSE,trace=0, dowarn=FALSE)
##    ans<-optimx(st, pr, pr.g, method="Rvmmin", control=ctrl)
##    cc<-coef(ans)
##    cat(n,"\t",(1/n),"\n")
##    print(cc)
##    n <- n + 1
## }


## ----label=C13simpleproduct2, echo=FALSE---------------------------------
require(optimx, quietly=TRUE) 
# load a range of optimization methods
pr <- function(y) {
- prod(y)*(1-sum(y))
}
pr.g <- function(x) {
g<- -prod(x)*(1-sum(x))/x + prod(x)
}
n<-2
cat("  n","\t","  x1","\t\t\t"," 1e5*(x1-1/n)\n")
options(digits=6)
while (n <= 6) {
   st<-1:(n-1)/(n*n)
   ctrl<-list(starttests=FALSE,trace=0, dowarn=FALSE)
   ans<-optimx(st, pr, pr.g, method="Rvmmin", control=ctrl)
   cc<-coef(ans)
   cat(n,"\t",(1/n),"\n")
   print(cc)
   n <- n + 1
}


## ----C13gabornll, echo=TRUE, cache=TRUE----------------------------------
nll <- function(y) {
  if ((any(y <= 10*.Machine$double.xmin)) || (sum(y)>1-.Machine$double.eps))
         .Machine$double.xmax
  else   - sum(log(y)) - log(1-sum(y))
}
nll.g <- function(y) { - 1/y + 1/(1-sum(y))} # so far not safeguarded
n<-1000
cat("  n","\t","  x1","\t\t\t"," 1e5*(x1-1/n)\n")
options(digits=6)
while (n <= 10000) {
   st<-1:(n-1)/(n*n)
   ctrl<-list(starttests=FALSE,trace=0, dowarn=FALSE)
   ans<-optimx(st, nll, nll.g, method="Rcgmin", control=ctrl)
   cc<-coef(ans)
   cat(n,"\t",(1/n),"\n")
   print(cc[1:5])
   n <- n + 1000
}


## ----label=C13badruns1, echo=TRUE----------------------------------------
require(optimx, quietly=TRUE)
n<-5
mset<-c("L-BFGS-B", "BFGS", "CG", "spg", "ucminf", "nlm", "nlminb", "Rvmmin", "Rcgmin")
a5<-optimx(2:n/n^2, nll, method=mset, control=list(dowarn=FALSE))
a5g<-optimx(2:n/n^2, nll, nll.g, method=mset, control=list(dowarn=FALSE))
a5gb<-optimx(2:n/n^2, nll, nll.g, lower=0, upper=1, method=mset, control=list(dowarn=FALSE))
summary(a5,order=value)
summary(a5g,order=value)
summary(a5gb,order=value)


## ----label=C13ravi1, echo=TRUE-------------------------------------------
require(BB, quietly=TRUE)
nllrv <- function(x) {- sum(log(x))}
nllrv.g <- function(x) {- 1/x }
proj <- function(x) {x/sum(x)}
n <- 5
aspg <- spg(par=(1:n)/n^2, fn=nllrv, gr=nllrv.g, project=proj)
aspgn <- spg(par=(1:n)/n^2, fn=nllrv, project=proj)
cat("F_optimal: with gradient=",aspg$value,"  num. approx.=",aspgn$value,"\n")
pbest<-rep(1/n, n)
cat("fbest = ",nllrv(pbest),"  when all parameters = ", pbest[1],"\n")
cat("deviations:  with gradient=",max(abs(aspg$par-pbest)),"   num. approx.=",max(abs(aspg$par-pbest)),"\n")

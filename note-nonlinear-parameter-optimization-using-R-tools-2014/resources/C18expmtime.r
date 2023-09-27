## ----label=C18expmtime0, echo=TRUE---------------------------------------
require(expm)
tt <- c(0.77, 1.69, 2.69, 3.67, 4.69, 5.71, 7.94, 9.67, 11.77, 17.77,
 23.77, 32.77, 40.73, 47.75, 54.90, 62.81, 72.88, 98.77, 125.92, 160.19,
 191.15, 223.78, 287.70, 340.01, 340.95, 342.01)
y <- c(1.396, 3.784, 5.948, 7.717, 9.077, 10.100, 11.263, 11.856, 12.251, 12.699,
 12.869, 13.048, 13.222, 13.347, 13.507, 13.628, 13.804, 14.087, 14.185, 14.351,
 14.458, 14.756, 15.262, 15.703, 15.703, 15.703)
ones<-rep(1,length(t))

Mpred <- function(theta) { # WARNING: assumes tt global
kvec<-exp(theta[1:3])
k1<-kvec[1]
k2<-kvec[2]
k3<-kvec[3]
#   MIN problem terbuthylazene disappearance
    z<-k1+k2+k3
    y<-z*z-4*k1*k3
    l1<-0.5*(-z+sqrt(y))
    l2<-0.5*(-z-sqrt(y))
    val<-100*(1-((k1+k2+l2)*exp(l2*tt)-(k1+k2+l1)*exp(l1*tt))/(l2-l1))
} # val should be a vector if t is a vector

negll <- function(theta){
# non expm version JN 110731
  pred<-Mpred(theta)
  sigma<-exp(theta[4])
  -sum(dnorm(y,mean=pred,sd=sigma, log=TRUE))
}

nlogL<-function(theta){
    k<-exp(theta[1:3])
    sigma<-exp(theta[4])
    A<-rbind(
             c(-k[1], k[2]),
             c( k[1], -(k[2]+k[3]))
             )
    x0<-c(0,100)
    sol<-function(tt) { 100-sum(expm(A*tt)%*%x0) }
    pred <- sapply(tt,sol)
    -sum(dnorm(y,mean=pred,sd=sigma, log=TRUE))
}
mytheta<-c(-2,-2,-2,-2)
vnlogL<-nlogL(mytheta)
cat("nlogL(-2,-2,-2,-2) = ", vnlogL, "\n")
vnegll<-negll(mytheta)
cat("negll(-2,-2,-2,-2) = ", vnegll, "\n")


## ----label=C18expmtime1, echo=FALSE--------------------------------------
## system("Rscript --vanilla supportdocs/tuning/timenegll.R >tempout.txt")
mntim<-dget(file="/home/john/nlpor/supportdocs/tuning/mntim.dput")
mntim

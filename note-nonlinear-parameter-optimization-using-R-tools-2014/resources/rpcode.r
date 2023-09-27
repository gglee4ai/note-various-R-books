require(Rvmmin)
sqs<-function(x) { 
   sum( seq_along(x)*(x - 0.5*seq_along(x))^2)
}
sqsg<-function(x) {
   ii<-seq_along(x)
   g<-2.*ii*(x - 0.5*ii)
}
xstrt<-rep(pi,200)
aa<-0.5*seq_along(xstrt)
ans<-Rvmmin(xstrt, sqs, gr=sqsg)

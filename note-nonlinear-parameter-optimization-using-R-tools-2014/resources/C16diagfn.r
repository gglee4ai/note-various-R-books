## ----label=C16diagfn0, echo=TRUE-----------------------------------------
diagfn<-function(par,skew=1) {
   x<-par[1]*skew
   y<-par[2]
   a<-x+y-2
   b<-x-y+2
   fval<-a*a+4*b*b
}
diaggr<-function(par,skew=1) {
   x<-par[1]*skew
   y<-par[2]
   a<-x+y-2
   b<-x-y+2
   g1<-(2*a+8*b)*skew
   g2<-2*a-8*b
   gr<-c(g1,g2)
   return(gr)
}


## ----label=C16diagfn2, echo=FALSE----------------------------------------
anssum<-function(ans){
	parv<-ans$par
	val<-ans$value
	nf<-ans$counts[1]
	ng<-ans$counts[2]
	ccode<-ans$convergence
	skew<-ans$skew
	all<-c(skew,val,parv,nf,ng,ccode)
	allnames<-c("skew", "fmin", "par1", "par2", "nf", "ng", "ccode")
        names(all)<-allnames
        all
}

cat("Diagonal function test\n")
s0<-c(1,1) # Start with both parameters 1
an1<-optim(s0,diagfn,skew=1,control=list(abstol=1e-15,reltol=1e-15))
an10<-optim(s0,diagfn,skew=10,control=list(abstol=1e-15,reltol=1e-15))
an100<-optim(s0,diagfn,skew=100,control=list(abstol=1e-15,reltol=1e-15))

cat("Nelder-Mead results\n")

an1$skew<-1
an10$skew<-10
an100$skew<-100
aat<-rbind(anssum(an1), anssum(an10), anssum(an100))
row.names(aat)<-c("an1","an1g", "an10")
print(aat)
cat("\n")
cat("gradient an1:")
print(diaggr(an1$par))
cat("gradient an10:")
print(diaggr(an10$par))
cat("gradient an100:")
print(diaggr(an100$par))


abf1<-optim(s0,diagfn,skew=1,method='BFGS')
abf1$skew=1
abf10<-optim(s0,diagfn,skew=10,method='BFGS')
abf10$skew=10
abf100<-optim(s0,diagfn,skew=100,method='BFGS')
abf100$skew=100
abf1g<-optim(s0,diagfn,gr=diaggr,skew=1,method='BFGS')
abf1g$skew=1
abf10g<-optim(s0,diagfn,gr=diaggr,skew=10,method='BFGS')
abf10g$skew=10
abf100g<-optim(s0,diagfn,gr=diaggr,skew=100,method='BFGS')
abf100g$skew=100
# cat("\n")
cat("\n BFGS results\n")
aatb<-rbind(anssum(abf1), anssum(abf1g), anssum(abf10), anssum(abf10g), anssum(abf100), anssum(abf100g))
row.names(aatb)<-c("abf1","abf1g", "abf10", "abf10g", "abf100", "abf100g")
print(aatb)

cat("\n\n")
cat("gradient abf1:")
print(diaggr(abf1$par))
cat("gradient abf1g:")
print(diaggr(abf1g$par))
cat("gradient abf10:")
print(diaggr(abf10$par))
cat("gradient abf10g:")
print(diaggr(abf10g$par))
cat("gradient abf100:")
print(diaggr(abf100$par))
cat("gradient abf100g:")
print(diaggr(abf100g$par))


## ----label=C16diagplot, echo=TRUE, fig.width=6, fig.height=6-------------
x = seq(-15,15, 0.5)
y = seq(-15,15, 0.5)
## skew=1
nx<-length(x)
ny<-length(y)
z<-matrix(NA,nx, ny)
z10<-z
z100<-z
k<-0
for (i in 1:nx) {
    for (j in 1:ny) {
       par<-c(x[i],y[j])
       val<-diagfn(par,skew=1)
       val10<-diagfn(par,skew=10)
       val100<-diagfn(par,skew=100)
	z[i,j]<-val
	z10[i,j]<-val10
	z100[i,j]<-val100
    }
}
persp(x, y, z10, theta = 20, phi = 10, expand = 0.5, col = "lightblue",
           ltheta = 120, shade = 0.75, ticktype = "detailed",
           xlab = "X", ylab = "Y", zlab = "Z") 

title("Diagonal function for skew = 10")

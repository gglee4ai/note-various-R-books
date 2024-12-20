# Protected log function
mlp<-function(x) {
   if (x < 1e-9) fval<- 0.1*.Machine$double.xmax
   else fval<-(log(x)*(x*x-3000)) 
}
ml<-function(x) {fval<-(log(x)*(x*x-3000)) }
min<-optimize(mlp, interval=c(-5, 5))
print(min)
minx<-optimize(ml, interval=c(-5, 5))
print(minx)
## But we missed the solution!
min2<-optimize(mlp, interval=c(-5,55))
print(min2)

## ----C21gnmhobbsb, echo=TRUE---------------------------------------------
require(gnm, quietly=TRUE)
y0 <- c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443) 
y0 <- c(y0, 38.558, 50.156, 62.948, 75.995, 91.972)
t0<-1:12
Hdta<-data.frame(y=y0, x=t0)
formula <- y ~ -1 + Mult(1, Inv(Const(1) + Exp(Mult(1 + offset(-x), Inv(1))))) 
st <- c(Asym=200, xmid=10, scal=3)
ans<-gnm(formula, start=st, data=Hdta)
ans
require(nlmrt, quietly=TRUE)
anls<-nlxb(y~Asym/(1+exp((xmid - x)/scal)), start=st , data=Hdta)
anls

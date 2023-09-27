## ----label=C09rb, echo=TRUE----------------------------------------------
require(optimx)
fr <- function(x) { ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
frg <- function(x) {   ## Rosenbrock Banana function gradient
    x1 <- x[1]
    x2 <- x[2]
    g1<- -400*(x2-x1*x1)*x1 - 2*(1-x1)
    g2<- 200*(x2-x1*x1)
    gg<-c(g1,g2)
}
sstart<-c(-1.2, 1)
arb<-optimx(par=sstart, fn=fr, gr=frg, method="all")
print(summary(arb, order=value))

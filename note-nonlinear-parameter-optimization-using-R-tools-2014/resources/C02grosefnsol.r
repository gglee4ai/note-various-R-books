## ----label=C02grosefnsol, echo=TRUE, cache=TRUE--------------------------

grose.f<- function(x, gs=100.){ # 1 to (n-1) variant of generalized rosenbrock function
    n <- length(x)
    1 + sum (gs*(x[1:(n-1)] - x[2:n]^2)^2 + (x[1:(n-1)] - 1)^2)    
}

grose.g<-function(x, gs=100.){ # gradient of 1 to (n-1) variant of generalized rosenbrock function
## vectorized by JN 090409
    n <- length(x)
    gg <- as.vector(rep(0,n))
    tn <- 2:n
    tn1 <- tn - 1
    z1<-x[tn1]-x[tn]^2
    z2<-x[tn1]-1
    gg[tn1]<-2.0*(z2+gs*z1)
    gg[tn]<-gg[tn]-4.0*gs*z1*x[tn]
    gg
}
source("C02steepdesc.R")
x <- c(0.1, 0.8)
arksd <- stdesc(x, grose.f, grose.g, control=list(trace=0))
print(arksd)


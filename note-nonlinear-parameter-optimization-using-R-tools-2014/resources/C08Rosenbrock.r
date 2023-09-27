## ----label=C08rosenbrock, echo=TRUE, cache=TRUE--------------------------
fr <- function(x) {   ## Rosenbrock Banana function
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
sstart<-c(-1.2, 1)


## ----label=C08prtopt, echo=TRUE, cache=TRUE------------------------------
prtopt<-function(optres, headname){ # compact print of optim() result
    cat(headname,": f(")
    npar<-length(optres$par)
    for (i in 1:(npar-1)) { cat(optres$par[[i]],", ") }
    cat(optres$par[[npar]],") = ",optres$value," after ",optres$counts[[1]],"f & ",optres$counts[[2]],"g\n")
    cat(optres$message,"  convergence code=",optres$convergence,"\n")
    tmp<-readline("continue")
}


## ----label=C08roseoptim, echo=TRUE, cache=TRUE---------------------------
anm<-optim(sstart, fr) # Nelder Mead is the default method
prtopt(anm, "anm")
abfgs<-optim(sstart, fr, method="BFGS")
prtopt(abfgs, "abfgs")
acg1<-optim(sstart, fr, method="CG", control=list(type=1))
prtopt(acg1, "acg1")
acg2<-optim(sstart, fr, method="CG", control=list(type=2))
prtopt(acg2, "acg2")
acg3<-optim(sstart, fr, method="CG", control=list(type=3))
prtopt(acg3, "acg3")
albfgsb<-optim(sstart, fr, method="L-BFGS-B")
prtopt(albfgsb, "albfgsb")


## ----label=C08roseng, echo=TRUE, cache=TRUE------------------------------
frg <- function(x) {   ## Rosenbrock Banana function gradient
    x1 <- x[1]
    x2 <- x[2]
    g1<- -400*(x2-x1*x1)*x1 - 2*(1-x1)
    g2<- 200*(x2-x1*x1)
    gg<-c(g1,g2)
}

## test gradient at standard start:
require(numDeriv)
gn<-grad(fr,sstart)
ga<-frg(sstart)
## max(abs(ga-gn))
max(abs(ga-gn))
## analytic gradient results
gabfgs<-optim(sstart, fr, frg, method="BFGS")
prtopt(gabfgs,"BFGS w. frg")
gacg1<-optim(sstart, fr, frg, method="CG", control=list(type=1))
prtopt(gacg1,"CG Fletcher-Reeves w. frg")
gacg2<-optim(sstart, fr, frg, method="CG", control=list(type=2))
prtopt(gacg2,"CG Polak-Ribiere w. frg")
gacg3<-optim(sstart, fr, frg, method="CG", control=list(type=3))
prtopt(gacg3,"CG Beale-Sorenson w. frg")
galbfgsb<-optim(sstart, fr, method="L-BFGS-B")
prtopt(galbfgsb,"L-BFGS-B w. frg")

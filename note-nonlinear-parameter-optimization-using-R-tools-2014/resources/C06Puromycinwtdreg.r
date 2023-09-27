## ----label=C06wtdnlls1, echo=TRUE, cache=TRUE----------------------------
## weighted nonlinear regression
Treated <- as.data.frame(Puromycin[Puromycin$state == "treated", ])

weighted.MM <- function(resp, conc, Vm, K)
{
    ## Purpose: exactly as white book p. 451 -- RHS for nls()
    ##  Weighted version of Michaelis-Menten model
    ## ----------------------------------------------------------
    ## Arguments: 'y', 'x' and the two parameters (see book)
    ## ----------------------------------------------------------
    ## Author: Martin Maechler, Date: 23 Mar 2001
    
    pred <- (Vm * conc)/(K + conc)
    (resp - pred) / sqrt(pred)
}

wtMM<-function(x, resp, conc){ # redefined for nlfb()
    Vm<-x[1]
    K <-x[2]
    res<-weighted.MM(resp, conc, Vm, K)
}

start <- list(Vm = 200, K = 0.1)

Pur.wt <- nls( ~ weighted.MM(rate, conc, Vm, K), data = Treated,
        start)
print(summary(Pur.wt))

require(nlmrt)
anlf<-nlfb(start, resfn=wtMM, jacfn=NULL, trace=FALSE, 
         conc=Treated$conc, resp=Treated$rate)
anlf

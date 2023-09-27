#  C11flbpex.R
require(optimx)
flb <- function(x) { # include this again so script is self-standing
     p <- length(x)
     sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) 
}

flbp <- function(x, flo, fup) { 
     p <- length(x)
     val <- sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) 
     if (any(x < flo) || any(x > fup)) val <- 1e300
     val
}
start<-rep(2,4)
lo<-rep(2,4)
up<-rep(4,4)
ans4p<-optimx(start, fn=flbp, method=c("hjkb", "Nelder-Mead", "nmkb"), flo=lo, fup=up)
print(summary(ans4p))




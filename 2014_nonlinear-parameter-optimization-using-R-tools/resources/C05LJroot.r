## ----label=C05lj2, echo=TRUE, cache=TRUE---------------------------------
ljgr<-function(r, ep=0.6501696, sig=0.3165555){
    expr1 <- 4 * ep; expr2 <- sig/r; expr8 <- sig/r^2
    value <- expr1 * (expr2^12 - expr2^6)
    grad <- -(expr1 * (12 * (expr8 * expr2^(11)) - 6 * (expr8 * expr2^5)))
}
root<-uniroot(ljgr, interval=c(0.001, 5))
cat("f(", root$root,")=",root$f.root," after ", root$iter," Est. Prec'n=",root$estim.prec,"\n")

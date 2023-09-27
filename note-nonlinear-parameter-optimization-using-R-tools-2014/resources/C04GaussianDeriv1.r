## ----label=C04chunk9, echo=TRUE, size="scriptsize", cache=TRUE-----------
cat("Gaussian derivative\n")
der<-function(x,mu,sigma){
    dd<-(-1)*(1/sqrt(2 * pi * sigma^2)) * (exp(-(x - mu)^2/(2 * sigma^2)) * 
    ((x - mu)/(sigma^2)))
}
r1<-uniroot(der, lower=1, upper=6, mu=4, sigma=1)
r.1<-uniroot(der, lower=1, upper=6, mu=4, sigma=.1)
r.01<-uniroot(der, lower=1, upper=6, mu=4, sigma=.01)
r.001<-uniroot(der, lower=1, upper=6, mu=4, sigma=.001)
sig<-c(1, .1, .01, .001)
roo<-c(r1$root, r.1$root, r.01$root, r.001$root)
tabl<-data.frame(sig, roo)
print(tabl)

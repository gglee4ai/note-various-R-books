## ----label=C10aderiv2, echo=TRUE, cache=TRUE-----------------------------
resfn<-function(pars, t=t, y=y){
  b1<-pars[[1]]
  b2<-pars[[2]]
  b3<-pars[[3]]
    .expr1 <- 100 * b1
    .expr2 <- 10 * b2
    .expr6 <- exp(-0.1 * b3 * t)
    .expr8 <- 1 + .expr2 * .expr6
    .expr13 <- .expr8^2
    .value <- .expr1/.expr8
    .grad <- array(0, c(length(.value), 3L), list(NULL, c("b1", 
        "b2", "b3")))
    .grad[, "b1"] <- 100/.expr8
    .grad[, "b2"] <- -(.expr1 * (10 * .expr6)/.expr13)
    .grad[, "b3"] <- .expr1 * (.expr2 * (.expr6 * (0.1 * t)))/.expr13
    attr(.value, "gradient") <- .grad
    .value
  res<-.value - y
  attr(res, "jacobian")<-.grad
  res
}
## Test the function
ydat<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443, 
          38.558, 50.156, 62.948, 75.995, 91.972) # for testing
tdat<-1:length(ydat) # for testing
start<-c(1,1,1)
myres<-resfn(start, y=ydat, t=tdat)
print(as.vector(myres)) # we don't want the "gradient"
ss<-as.numeric(crossprod(as.vector(myres)))
ss
JJ<-attr(myres,"jacobian")
JJ
svd(JJ)$d
grad<-crossprod(JJ,as.vector(myres))
print(as.vector(grad))

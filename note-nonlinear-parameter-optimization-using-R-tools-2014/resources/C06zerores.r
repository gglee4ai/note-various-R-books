## ----label=C06zeroresprob1, echo=TRUE, cache=TRUE------------------------
  require(nlmrt)
  x <- 1:10
  y <- 2*x + 3                            # perfect fit
  yeps <- y + rnorm(length(y), sd = 0.01) # added noise
  anoise <- nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321))
  summary(anoise)
  aperf <- try(nls(y ~ a + b*x, start = list(a = 0.12345, b = 0.54321)))
  print(strwrap(aperf))
  ldata<-data.frame(x=x, y=y)
  aperfn <- try(nlxb(y ~ a + b*x, start = list(a = 0.12345, b = 0.54321), data=ldata))
  aperfn

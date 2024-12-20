## C06comparewtloss.R
source("C06nls-wtloss.R", echo=FALSE) # to recover wlnls
require(nlmrt)
wlmod <-  "Weight ~ b0 + b1*2^(-Days/th)"
wlnlxb <-  nlxb(wlmod, data=wl, start=c(b0=1, b1=1, th=1))
# note the different outputs below 
# bare
wlnlxb
# print()
print(wlnlxb)
# summary()
summary(wlnlxb)

## coefficients
coef(wlnls)
coef(wlnlxb)

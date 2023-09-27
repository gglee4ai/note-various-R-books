## ----label=C04mytanroot1
source("C04mytangent.R")
require("rootoned")
tint<-c(80,100)
ru<-uniroot(mytan, tint)
ru
rz<-zeroin(mytan, tint)
rz
rr<-root1d(mytan, tint)
rr
rn80<-newt1d(mytan, gmytan, 80)
rn80
rn100<-newt1d(mytan, gmytan, 100)
rn100

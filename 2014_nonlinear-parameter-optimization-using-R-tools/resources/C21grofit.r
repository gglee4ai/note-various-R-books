## ----C21tgrofit, echo=TRUE-----------------------------------------------
## tgrofit.R -- Use Hobbs problem to test grofit
    y<-c(5.308, 7.24, 9.638, 12.866, 17.069, 23.192, 31.443) 
    y<-c(y, 38.558, 50.156, 62.948, 75.995, 91.972)
    tt<-1:12
require(grofit, quietly=TRUE)
ah <- gcFitModel(time=tt, data=y)
print(summary(ah))
ah$model
summary(ah$nls)
ah$nls

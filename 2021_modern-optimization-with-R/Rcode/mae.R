### mae.R file ###

library(reticulate) # load the package

# the simple MAE function:
mae_R=function(y,x) mean(abs(y-x))

source_python("mae.py")  # source the python script

a=c(0,1,0.5); b=c(1,1,1) # two numeric vectors
mae1=mae_py(a,b)         # compute mae using python
mae2=mae_R(a,b)          # compute mae using R

cat("the mae is",mae1,"=",mae2,"\n")


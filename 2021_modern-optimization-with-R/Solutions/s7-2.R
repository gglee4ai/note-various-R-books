### code copied from tsf.R file:
# get sunspot yearly series:
url="http://www.sidc.be/silso/DATA/SN_y_tot_V2.0.txt"
series=read.table(url)
# lets consider data from the 1700-2019 years:
# lets consider column 2: sunspot numbers
series=series[1:320,2]
# save to CSV file, for posterior usage (if needed)
write.table(series,file="sunspots.csv",
            col.names=FALSE,row.names=FALSE)

L=length(series) # series length
forecasts=32 # number of 1-ahead forecasts 
outsamples=series[(L-forecasts+1):L] # out-of-samples
sunspots=series[1:(L-forecasts)] # in-samples
LIN=length(sunspots) # length of in-samples

# mean absolute error of residuals
maeres=function(residuals) mean(abs(residuals))

INIT=10 # initialization period (no error computed before)
library(forecast) # load forecast package
arima=auto.arima(sunspots) # detected order is AR=2, MA=1
print(arima) # show ARIMA model
cat("arima fit MAE=",
    maeres(arima$residuals[INIT:length(sunspots)]),"\n")

### s7-2.R solution code:
library(pso) # load pso

# evaluation function of arma coefficients:
# s is a vector with 4 real values 
# (ar1,ar2,ma1,intercept or m)
evalarma=function(s)
{ a=suppressWarnings(arima(sunspots,order=c(AR,0,MA),fixed=s))
  R=a$residuals[INIT:length(sunspots)]
  R=maeres(R)
  if(is.nan(R)) R=Inf # death penalty
  return(maeres(R))
}

AR=2;MA=1
maxit=50; LP=20
meants=mean(sunspots);K=0.1*meants
lower=c(rep(-1,(AR+MA)),meants-K)
upper=c(rep(1,(AR+MA)),meants+K)
C=list(maxit=maxit,s=LP,trace=10,REPORT=10)
set.seed(12345) # set for replicability
PSO=psoptim(rep(NA,length(lower)),fn=evalarma,
               lower=lower,upper=upper,control=C)
arima2=arima(sunspots,order=c(AR,0,MA),fixed=PSO$par)
print(arima2)
cat("pso fit MAE=",PSO$value,"\n")

# one-step ahead predictions:
f1=rep(NA,forecasts)
f2=rep(NA,forecasts)
for(h in 1:forecasts)
  { # execute arima with fixed coefficients but with more in-samples:
   # normal ARIMA forecasts:
   arima1=arima(series[1:(LIN+h-1)],order=arima$arma[c(1,3,2)],fixed=arima$coef)
   f1[h]=forecast(arima1,h=1)$mean[1]
   # PSO ARIMA forecasts:
   arima2=arima(series[1:(LIN+h-1)],order=arima2$arma[c(1,3,2)],fixed=arima2$coef)
   f2[h]=forecast(arima2,h=1)$mean[1]
  }
e1=maeres(outsamples-f1)
e2=maeres(outsamples-f2)
text1=paste("arima (MAE=",round(e1,digits=1),")",sep="")
text2=paste("pso arima (MAE=",round(e2,digits=1),")",sep="")

# show quality of one-step ahead forecasts: 
ymin=min(c(outsamples,f1,f3))
ymax=max(c(outsamples,f1,f3))
plot(outsamples,ylim=c(ymin,ymax),type="l",
     xlab="time",ylab="values")
lines(f1,lty=2,type="b",pch=3,cex=0.5)
lines(f2,lty=3,type="b",pch=5,cex=0.5)
legend("topright",c("sunspots",text1,text2),lty=1:3,pch=c(1,3,5))

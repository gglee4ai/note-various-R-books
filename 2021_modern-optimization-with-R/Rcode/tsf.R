### tsf.R file ###

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

# mean absolute error of residuals
maeres=function(residuals) mean(abs(residuals))

# fit best ARIMA model:
INIT=10 # initialization period (no error computed before)
library(forecast) # load forecast package
arima=auto.arima(sunspots) # detected order is AR=2, MA=1
print(arima) # show ARIMA model
cat("arima fit MAE=",
    maeres(arima$residuals[INIT:length(sunspots)]),"\n")

# one-step ahead forecasts:
# (this code is needed because forecast function
#  only performs h-ahead forecasts)
LIN=length(sunspots) # length of in-samples
f1=rep(NA,forecasts)
for(h in 1:forecasts)
  { # arima with fixed coefficients and more in-samples:
   arima1=arima(series[1:(LIN+h-1)],
           order=arima$arma[c(1,3,2)],fixed=arima$coef)
   f1[h]=forecast(arima1,h=1)$mean[1]
  }
e1=maeres(outsamples-f1)
text1=paste("arima (MAE=",round(e1,digits=1),")",sep="")

### evolutionary methods: 
# evaluate time series function
#   receives f: a function (GP) or expression (GE) 
#   if(h>0) then returns 1-ahead forecasts
#   else returns MAE over fitting period (in-samples)
evalts=function(f,h=0) # fitness function
{ # global object previously defined: series,LIN 
  if(h>0) y=series else y=series[1:LIN]
  LTS=length(y)
  F=rep(0,LTS) # forecasts
  E=rep(0,LTS) # residuals
  if(h>0) I=(LTS-h+1):LTS # h forecasts
  else I=INIT:LTS # fit to in-samples 
  for(i in I)
    { 
     if(class(f)=="function") F[i]=f(y[i-1],y[i-2]) 
     else F[i]=eval(f) # expr includes: y[i-1] and y[i-2]
     if(is.nan(F[i])) F[i]=0 # deal with NaN
     E[i]=y[i]-F[i]
    }
  if(h>0) return (F[I]) # forecasts
  else return(maeres(E[I])) # MAE on fit
}

Pop=200; Maxit=500; Eli=1 # common setup

# fit genetic programming (GP) arithmetic model:
RGP=require(rgp) # load rgp
if(RGP){ # this code is only executed if rgp is installed
 ST=inputVariableSet("y1","y2") # order of AR component
 cF1=constantFactorySet(function() rnorm(1)) # mean=0, sd=1
 FS=functionSet("+","*","-","/") # arithmetic
 # GP time series function
 #   receives function f 
 #   if(h>0) then returns 1-ahead forecasts
 #   else returns MAE over fitting period (in-samples)
 gpmut=function(func) # GP mutation function
 { mutateSubtree(func,funcset=FS,inset=ST,conset=cF1,
                mutatesubtreeprob=0.3,maxsubtreedepth=4)}
 set.seed(12345) # set for replicability
 # run the GP:
 cat("run GP:\n")
 gp=geneticProgramming(functionSet=FS,inputVariables=ST,
       constantSet=cF1,populationSize=Pop,eliteSize=Eli,
       fitnessFunction=evalts,
       stopCondition=makeStepsStopCondition(Maxit),
       mutationFunction=gpmut,verbose=TRUE)
 f2=evalts(gp$population[[which.min(gp$fitnessValues)]],
         h=forecasts)
 e2=maeres(outsamples-f2)
 text2=paste("gp (MAE=",round(e2,digits=1),")",sep="")
 cat("best solution:\n")
 print(gp$population[[which.min(gp$fitnessValues)]])
 cat("gp fit MAE=",min(gp$fitnessValues),"\n")
}

# fit grammatical evolution (GE) arithmetic model:
GE=require(gramEvol) # 
if(GE) # this code is only executed if gramEvol is installed
{ # set the grammar rules:
 ruleDef=list(expr=gsrule("<expr><op><expr2>","<expr2>"),
             op=gsrule("+","-","*","/"), 
             expr2=gsrule("y[i-1]","y[i-2]","<value>"),
             value=gsrule("<digits>.<digits>"),
             digits=gsrule("<digits><digit>","<digit>"),
             digit=grule(0,1,2,3,4,5,6,7,8,9)
            )
 gDef=CreateGrammar(ruleDef) # grammar object
 # simple monitoring function:
 monitor=function(results)
 { # print(str(results)) shows all results components
  iter=results$population$currentIteration # current iteration 
  f=results$best$cost # best fitness value
  if(iter==1||iter%%100==0) # show 1st and every 100 iter
    cat("iter:",iter,"f:",f,"\n")
 }
 set.seed(12345) # set for replicability
 # run the GE:
 cat("run GE:\n")
 ge=GrammaticalEvolution(gDef,evalts,optimizer="es",
                         popSize=Pop,elitism=Eli,
                         iterations=Maxit,monitorFunc=monitor)
 b=ge$best # best solution
 cat("best solution:\n")
 print(b$expression)
 cat("ge fit MAE=",b$cost,"\n")
 f3=evalts(b$expressions,h=forecasts)
 e3=maeres(outsamples-f3)
 text3=paste("ge (MAE=",round(e3,digits=1),")",sep="")
}

# show quality of one-step ahead forecasts: 
ymin=min(c(outsamples,f1))
if(RGP) ymin=min(ymin,f2)
if(GE) ymin=min(ymin,f3)
ymax=max(c(outsamples,f1))
if(RGP) ymax=max(ymax,f2)
if(GE) ymax=max(ymax,f3)
pdf("fsunspots.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
plot(outsamples,ylim=c(ymin,ymax),type="b",pch=1,
     xlab="time (years after 1988)",ylab="values",cex=0.8)
lines(f1,lty=2,type="b",pch=2,cex=0.5)
pch=1:2;lty=1:2;text=c("sunspots",text1)
if(RGP) 
 { lines(f2,lty=3,type="b",pch=3,cex=0.5)
   lty=c(lty,3);pch=c(pch,3)
   text=c(text,text2)
 }
if(GE) 
 { lines(f3,lty=4,type="b",pch=4,cex=0.5)
   lty=c(lty,4);pch=c(pch,4)
   text=c(text,text3)
 }
legend("topright",text,lty=lty,pch=pch)
dev.off()

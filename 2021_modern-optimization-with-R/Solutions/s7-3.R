library(rminer) # load rminer package
library(mco) # load mco package

# load wine quality dataset directly from UCI repository:
file="http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
d=read.table(file=file,sep=";",header=TRUE) 

# convert the output variable into 2 classes of wine:
# "low" <- 3,4,5 or 6; "high" <- 7, 8 or 9
d$quality=cut(d$quality,c(1,6,10),c("low","high"))

# to speed up the demonstration, only 25% of the data is used:
n=nrow(d) # total number of samples
ns=round(n*0.25) # select a quarter of the samples
set.seed(12345) # for replicability
ALL=sample(1:n,ns) # contains 25% of the index samples
w=d[ALL,] # new wine quality data.frame
# show the attribute names:
cat("attributes:",names(w),"\n")
cat("output class distribution (25% samples):\n")
print(table(w$quality)) # show distribution of classes

# save dataset to a local CSV file: 
write.table(w,"wq25.csv",col.names=TRUE,row.names=FALSE,sep=";")

# holdout split into training (70%) and test data (30%):
H=holdout(w$quality,ratio=0.7)
cat("nr. training samples:",length(H$tr),"\n")
cat("nr. test samples:",length(H$ts),"\n")
# save to file the holdout split index:
save(H,file="wine-H.txt",ascii=TRUE)

output=ncol(w) # output target index (last column)
maxinputs=output-1 # number of maximum inputs

# auxiliary functions:
# rescale x from [0,1] to [min,max] domain:
transf=function(x,min,max) return (x*(max-min)+min)
# decode the x genome into the model hyperparameters:
# Model is a global variable with "randomForest" or "ksvm"
decode=function(x)
{ if(Model=="randomForest") 
  { # 2 hyperparameters:
    ntree=round(transf(x[1],1,200)) 
    mtry=round(transf(x[2],1,11)) 
    return(c(ntree,mtry))
  }
 else 
  { # 2 SVM hyperparameters:
    sigma=transf(x[1],2^-6,2^1)
    C=transf(x[2],2^-2,2^5) 
    return(c(sigma,C))
  }
}

# evaluation function (requires some computation):
# Model is a global variable with "randomForest" or "ksvm"
evalmodel=function(x) # x is a solution
{
 # read input features: from position 1 to maxinputs
 features=round(x[1:maxinputs]) # 0 or 1 vector
 inputs=which(features==1) # indexes with 1 values
 # use first feature if inputs is empty
 if(length(inputs)==0) inputs=1 
 J=c(inputs,output) # attributes
 k3=c("kfold",3,123) # internal 3-fold validation
 # read hyperparameters:
 hpar=decode(x[(maxinputs+1):length(x)])
 if(Model=="randomForest")
   M=suppressWarnings(try(
       mining(quality~.,w[H$tr,J],method=k3,
              model="randomForest",ntree=hpar[1],
              # mtry cannot be higher than I - the
              #  number of inputs
              mtry=min(hpar[2],length(inputs))
             ) 
                       ,silent=TRUE))
 else M=suppressWarnings(try(
       mining(quality~.,w[H$tr,J],method=k3,
              model="ksvm",kpar=list(sigma=hpar[1]),C=hpar[2])
                       ,silent=TRUE))

 # AUC for the internal 3-fold cross-validation:
 if(class(M)=="try-error") auc=0.5 # worst auc 
 else auc=as.numeric(mmetric(M,metric="AUC")) 
 auc1=1-auc # maximization into minimization goal
 ninputs=length(inputs) # number of input features
 EVALS<<-EVALS+1 # update evaluations
 if(EVALS==1||EVALS%%Psize==0) # show current evaluation:
    cat(EVALS," evaluations (AUC: ",round(auc,2),
        " nr.features:",ninputs,")\n",sep="")
 return(c(auc1,ninputs)) # 1-auc,ninputs
}

# NSGAII multi-objective optimization:
m=2 # two objectives: AUC and number of input features
hrf=2 # number of hyperparameters for randomForest 
genome=maxinputs+hrf # genome length
lower=rep(0,genome)
upper=rep(1,genome)
EVALS<<-0 # global variable
PTM=proc.time() # start clock
Psize=20 # population size

set.seed(12345) # for replicability
Model="randomForest"
cat("nsga2 for ",Model,":\n")
s1=mco::nsga2(fn=evalmodel,idim=length(lower),odim=m,
         lower.bounds=lower,upper.bounds=upper,
         popsize=Psize,generations=10)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")

set.seed(12345) # for replicability
EVALS<<-0 # global variable
Model="ksvm"
cat("nsga2 for ",Model,":\n")
s2=mco::nsga2(fn=evalmodel,idim=length(lower),odim=m,
         lower.bounds=lower,upper.bounds=upper,
         popsize=Psize,generations=10)
sec=(proc.time()-PTM)[3] # get seconds elapsed
cat("time elapsed:",sec,"\n")

# plot the Pareto fronts: 
par(mfrow=c(1,2))  # plotting area with 1*2 array
# 1st plot
po1=which(s1$pareto.optimal) # optimal points
# sort Pareto front according to f2 (number of features):
i1=sort.int(s1$value[po1,2],index.return=TRUE)
pareto1=s1$value[po1[i1$ix],] # Pareto front f1,f2 values
plot(1-pareto1[,1],pareto1[,2],xlab="AUC",
     ylab="nr. features",type="b",lwd=2,main="randomForest Pareto:")
# 2nd plot
po2=which(s2$pareto.optimal) # optimal points
# sort Pareto front according to f2 (number of features):
i2=sort.int(s2$value[po2,2],index.return=TRUE)
pareto2=s2$value[po2[i2$ix],] # Pareto front f1,f2 values
plot(1-pareto2[,1],pareto2[,2],xlab="AUC",
     ylab="nr. features",type="b",lwd=2,main="ksvm Pareto:")

### wineApp/app.R file ###

library(shiny) # load shiny
library(rminer) # load rminer

#---  global variables ------
# load the previously saved files
# (the files are in the upper level "../" directory):
w=read.table("../wq25.csv",header=TRUE,sep=";",
             stringsAsFactors=TRUE)
load("../wine-H.txt") # loads into H the holdout index 
load("../wine-s1.txt") # loads into s1 the Pareto front

# global objects related with the data (w):
output=ncol(w) # output target index (last column)
maxinputs=output-1 # number of maximum inputs
Y=w[H$ts,"quality"] # target test data

# global objects related with the Pareto front (s1):
po=which(s1$pareto.optimal) # optimal points
NP=length(po) # number of Pareto front points
# sort Pareto front according to f2 (number of features):
i=sort.int(s1$value[po,2],index.return=TRUE)
pareto=s1$value[po[i$ix],] # Pareto front f1,f2 values
pop=s1$par[po[i$ix],] # Pareto solutions

# User Interface (UI):
ui=fluidPage( # begin fluidPage
   titlePanel("wineApp"), # title of the panel
   sidebarLayout( # sidebar with input and outputs
      position = c("right"), # put input at the right
   sidebarPanel(  # panel for input
    numericInput(
      inputId="number",
      label=paste("Pareto front point (1 to ",NP,"):",sep=""), 
      min=1,  # minimum value
      max=NP, # maximum value
      step=1, # select only integers
      value=1 # default value
    )), # end sidebarPanel
   mainPanel( # panel for outputs
    h5("input features:"), # show fixed text
    verbatimTextOutput("text1"),  
    h5("hyper parameters:"), # show fixed text
    verbatimTextOutput("text2"),
    splitLayout( # show horizontally 2 plots
      cellWidths = 350, # enlarge plot size
      plotOutput(outputId="par"),
      plotOutput(outputId="roc")
    ) # end splitLayout
    ) # end mainPanel
  ) # end sidebar
) # end fluidPage

# Server function:
server=function(input,output) 
{
  # reactive component: return the selected model 
  #  (only executed once for each input$number change)
  modelInput=reactive({ selectmodel(input$number) })
  # output components:
  output$text1=renderText({ # show inputs
    S=modelInput()
    paste(S$ninputs) 
  }) 
  output$text2=renderText({ # show hyperparameters
    S=modelInput()
    paste(S$hpar)
  }) 
  output$par=renderPlot({ # plot Pareto curve
    plot(1-pareto[,1],pareto[,2],xlab="AUC",
         ylab="nr. features",type="b",lwd=2,
         main="Pareto curve:")
    points(1-pareto[input$number,1],pareto[input$number,2],
           pch="X",cex=1.5)
  })
  output$roc=renderPlot({ # plot ROC curve
    S=modelInput() 
    mgraph(Y,S$P,graph="ROC",main=S$main,
           Grid=10,baseline=TRUE,leg="xgboost")
  })
}

# auxiliary functions:
# rescale x from [0,1] to [min,max] domain:
transf=function(x,min,max) return (x*(max-min)+min)
# decode the x genome into the model hyperparameters:
decode=function(x)
{
 # 4 xgboost hyperparameters for default "gbtree":
 nrounds=round(transf(x[1],1,200)) # [1,200]
 eta=x[2] # [0.0,1.0]
 gamma=transf(x[3],0,10) # [0,10]
 max_depth=round(transf(x[4],0,12)) # {0,...,12}
 return(c(nrounds,eta,gamma,max_depth))
}
# select from the Pareto front the classifier
#  i - index of the sorted Pareto front
selectmodel=function(i)
{ 
  x=pop[i,] # selected genome
  # decode the model:
  features=round(x[1:maxinputs])
  inputs=which(features==1)
  ninputs=names(w)[inputs]
  J=c(inputs,output) # data attributes
  hpar=decode(x[(maxinputs+1):length(x)])
  if(pareto[i,1]==0.5) # random classifier
     P=cbind(rep(1,length(Y)),rep(0,length(Y))) # predict "low"
  else
   {
    M=fit(quality~.,w[H$tr,J],model="xgboost",
          nrounds=hpar[1],eta=hpar[2],
          gamma=hpar[3],max_depth=hpar[4])
    P=predict(M,w[H$ts,J]) # get xgboost predictions
   }
  auc=mmetric(Y,P,metric="AUC") # compute the AUC
  main=paste("test data ROC curve"," (AUC=",
            round(auc,digits=2),")",sep="")
 return(list(ninputs=ninputs,hpar=hpar,P=P,main=main))
}

# call to shinyApp (launches the app in browser):
shinyApp(ui=ui,server=server)

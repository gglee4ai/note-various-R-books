### demoApp/app.R file ###

library(shiny) # load the package

# User Interface (UI): with 2 inputs and 1 output
ui=fluidPage( # begin fluidPage
  titlePanel("demoApp"), # title of the panel
  sidebarLayout( # sidebar with input and outputs
  sidebarPanel(  # panel for inputs
    selectInput("var", # input: 3 character choices
      label="Choose a variable",
      choices=c("pH","alcohol","quality"),
      selected="pH" # default choice
    ),
    sliderInput( # input: numeric selection
      inputId="bins",
      label="number of bins:", 
      min=2,   # minimum value
      max=30, # maximum value
      value=10 # default value
    )), # end sidebarPanel
  mainPanel( # panel for outputs
      plotOutput(outputId="distPlot")
    )) # end sidebar
) # end fluidPage

# Server function:
server=function(input,output) 
{
  output$distPlot=renderPlot({
    x=w[,input$var] # select variable from w
    # draw the histogram with the specified number of bins
    text=paste("Histogram of",input$var) # histogram title
    hist(x,breaks=input$bins,col="gray",main=text)
  })
}

# load the wine data and create object w
file="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
w=read.table(file,sep=";",heade=TRUE)
# call to shinyApp
shinyApp(ui=ui,server=server)

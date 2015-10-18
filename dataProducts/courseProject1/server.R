library(shiny)
library(ggplot2)

#options(shiny.error=browser)



#read data for plot from .csv file
setClass('myDateFormat')
setAs('character', 'myDateFormat', function(from) as.POSIXct(from, format="%d/%m/%Y", origin="1970-01-01"))
d <- read.csv('data/bbAccuracy.csv', header = TRUE, colClasses = c('myDateFormat', 'numeric'))
d$attemptNumber <- seq(1,nrow(d))

getXDataInfo <- function(xDataSelect) {
  xDataByDate <- list(
    predictionRange = seq(d$Date[1],as.POSIXct("01/1/2050",format="%d/%m/%Y", origin="1970-01-01"), by = "days"),
    xData = d[['Date']]
  )
  xDataByShots <- list(
    predictionRange = seq(1:1e4),
    xData = d[['attemptNumber']]
  )
  
  if(xDataSelect == 'Date')
  {
    xDataByDate
  }
  else #xDataSelect == 'Number of attempts'
  {
    xDataByShots
  }
}

predictTenSunk <-function(polynomialDegree, xDataInfo){
  #Return when I am likely to sink all 10 shots, based on 
  #a polynomial of the specified degree, and given the provided
  #x-data to base the model on
  #just evaluates the model at all points in 'xDataInfo$predictionRange'
  #and returns the first value that predicts 10 or above
  cat('predictTenSunk: start\n')
  
  #prepare the model for prediction
  xData <- as.numeric(xDataInfo$xData)
  shotsSunk <- d$ShotsSunk 
  model <- lm(shotsSunk ~ poly(xData,polynomialDegree))
  
  #make 'xDataRange' a data frame as required by the 'predict' function
  xDataRange <- as.numeric(xDataInfo$predictionRange)
  xDataRange <- data.frame(xDataRange)
  xVarName <- all.vars(as.formula(model))[2]
  names(xDataRange) <- xVarName
  
  predictions <- data.frame(xVal = xDataRange)
  predictions$predictedYVal <- predict(model, newdata = xDataRange)
  
  firstOverTarget <- suppressWarnings(min(which(predictions$predictedYVal >= 10)))
  xDataInfo$predictionRange[firstOverTarget]
  # browser()
}

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  cat('SHINYSERVER: start\n')
  
  #helper function: return x-data for plot based on user selection
  reactiveXDataInfo <- reactive({getXDataInfo(input$xDataSelect)})
  
  reactivePrediction <- reactive(predictTenSunk(input$modelDegree, reactiveXDataInfo()))
  
  #plotting function: 
  makePlot <- function(modelDegree){
    cat("makePlot: start\n")
    
    xDataInfo = reactiveXDataInfo()
    
    qplot( x = xDataInfo$xData, 
           y = d$ShotsSunk, 
           geom = "point",
           # ylim = c(0,10),
           xlim = c(xDataInfo$xData[1], reactivePrediction())
    ) +
    geom_smooth(method='lm', formula = y ~ poly(x, modelDegree), fullrange=TRUE) + 
    coord_cartesian(ylim = c(0,10)) + 
    theme_classic()
  }
  
  output$select1 = renderText(input$xDataSelect)
  output$prediction = renderText(as.character(reactivePrediction()))
  output$bbplot = renderPlot(makePlot(input$modelDegree))
})

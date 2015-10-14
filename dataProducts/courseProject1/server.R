library(shiny)
library(ggplot2)
library(chemCal)

#options(shiny.error=browser)

makePlot <- function(xDataChoice, modelDegree){
  if (xDataChoice == 'Date')
  {
    xData <- 'Date'
  }
  else
  {
    xData <- 'attemptNumber'
  }
  
  m <- lm(ShotsSunk ~ poly(Date,3), data = d)
  p <- inverse.predict(m, newdata = data.frame(shotsSunk = 10))
  as.POSIXct(p$Prediction, origin = "1970-01-01")
  cat(as.character(as.POSIXct(p$Prediction, origin = "1970-01-01")))
  
  
  qplot(x = d[[xData]], y = d$ShotsSunk, geom = "point", ylim = c(0,10)) +
    geom_smooth(method='lm', formula = y ~ poly(x, modelDegree)) + 
    # geom_smooth(method='loess') + 
    theme_classic()
}


#read data for plot from .csv file
setClass('myDateFormat')
setAs('character', 'myDateFormat', function(from) as.POSIXct(from, format="%d/%m/%Y", origin="1970-01-01"))
d <- read.csv('data/bbAccuracy.csv', header = TRUE, colClasses = c('myDateFormat', 'numeric'))
d$attemptNumber <- seq(1,nrow(d))

# m <- lm(ShotsSunk ~ poly(Date,3), data = d)
# p <- inverse.predict(m, newdata = data.frame(shotsSunk = 10))
# as.POSIXct(p$Prediction, origin = "1970-01-01")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  output$select1 = renderText(input$xDataSelect)

  output$bbplot = renderPlot(makePlot(input$xDataSelect, input$modelDegree))
})


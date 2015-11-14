library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Basketball Accuracy"),
  
  sidebarPanel(
    
    h4('App description:'),
    p('How good am I at basketball??'),
    p('Each point on the plot is a record of one of my attempts to sink ten out of ten shots from a given spot on the court.'),
    p('The y-axis shows how many of the ten shots I sunk on a given attempt.'),
    p('The x-axis either shows the attempt number (e.g. for 5th attempt, x=5), or the date of the attempt - you can select which to use:'),
    selectInput('xDataSelect', 'Select plot x-axis data', c('Number of attempts', 'Date')),
    
    h4('My question to you:'),
    p('When will I shoot 10/10??'),
    p('The blue line on the plot shows a ploynomial fit to the data. This is used to predict when I will finally get a perfect score (see prediction printed under the plot).'),
    p('Use the slider below to set the degree of the polynomial to fit to the data:'),
    sliderInput('modelDegree', 'Select polynomial degree', 1, min = 1, max = 10, ticks = FALSE),
    
    p('Which model looks best? More importantly, when will I get a perfect score?'),
    
    img(src="http://www.jumpstartsports.com/upload/images/Radnor_Basketball/448650-basketball__mario_sports_mix_.png", height = 200, width = 200)
  ),
  
  mainPanel(
    plotOutput('bbplot'),
    h3("Model's prediction:"),
    textOutput('prediction')
  )
))

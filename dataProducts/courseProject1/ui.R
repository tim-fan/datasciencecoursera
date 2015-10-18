library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Shot Accuracy"),
  
  sidebarPanel(
    p('This is the sidebar'),
    selectInput('xDataSelect', 'Select plot x-axis data', c('Number of attempts', 'Date')),
    sliderInput('modelDegree', 'Select polynomial degree', 1, min = 1, max = 10),
    img(src="http://www.jumpstartsports.com/upload/images/Radnor_Basketball/448650-basketball__mario_sports_mix_.png", height = 200, width = 200)
  ),
  
  mainPanel(
    plotOutput('bbplot'),
    h3("Model's prediction:"),
    textOutput('prediction')
  )
))

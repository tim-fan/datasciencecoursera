library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  # Application title
  titlePanel("Shot Accuracy"),
  
  sidebarPanel(
    p('This is the sidebar'),
    selectInput('xDataSelect', 'Select plot x-axis data', c('Number of attempts', 'Date')),
    sliderInput('modelDegree', 'Select polynomial degree', 1, min = 1, max = 10)
  ),
  
  mainPanel(
    p('Here is the main panel'),
    p('You selected: '), textOutput('select1'),
    plotOutput('bbplot')
  )
))
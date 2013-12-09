library(shiny)
library(ggplot2)

dataset <- diamonds

shinyUI(pageWithSidebar(
  
  headerPanel("Explore SAPA's data!"),
  sidebarPanel(
    
    selectInput('x', 'Trait or Ability', names(dataset)),
    selectInput('y', 'Grouping Variable', names(dataset), names(dataset)[[2]]),
    helpText("Built by Jason A. French and Francis Smart."),
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))
library(shiny)
library(ggplot2)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("County Tweets"),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput(inputId = "category",
      label = "Select category label",
      choices = c("All", "News", "Junk"),
      selected = "All"),
    checkboxInput("rt", "Show Retweets", FALSE)
  ),

  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot"),

    
    tableOutput("tweet.table")
  )
))
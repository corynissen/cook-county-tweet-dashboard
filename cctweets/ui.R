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
      choices = c("All", "Gov", "Foursquare", "Jail", "News", "Sports", "Weather", "Junk"),
      selected = "All"),
    checkboxInput("rt", "Show Retweets", FALSE),
    br(),
    actionButton("refresh", "Click to Refresh Data")
  ),

  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("plot"),

    
    tableOutput("tweet.table")
  )
))

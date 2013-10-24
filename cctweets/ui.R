
library(shiny)
library(ggplot2)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Cook County Tweets"),
   
  sidebarPanel(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/styles.css"),
      tags$script(type = 'text/javascript', src = 'js/responsiveTable.js')
    ),
    selectInput(inputId = "category",
      label = "Select category label",
      choices = c("All", "Gov", "Foursquare", "Jail", "News", "Sports",
          "Weather", "Junk"),
      selected = "All"),    
    checkboxInput("rt", "Show Retweets", FALSE),
    br(),
    uiOutput("day.slider"),
    br(),
    textInput("search.term", "Subset Data By Search Term", "")
##    br(),
##    actionButton("refresh", "Click to Update Data (takes about a minute)")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Tweets",
        h3(textOutput("caption")),
        plotOutput("plot"),    
        tableOutput("tweet.table")
      ),
      tabPanel("Links",
        uiOutput(outputId = "links.freq.table"),
        tableOutput("links.table")
      ),
      tabPanel("Names",
        uiOutput(outputId = "names.freq.table"),
        tableOutput("names.table")
      )
    )
  )
))

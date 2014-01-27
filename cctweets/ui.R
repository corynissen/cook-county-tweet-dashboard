
library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Cook County Tweets"),
   
  sidebarPanel(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/styles.css"),
      tags$script(type = 'text/javascript', src = 'js/responsiveTable.js'),      
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 185px; }"),
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 280px; }"),
      tags$style(type='text/css', ".span4 { max-width: 280px; }")
    ),
    selectInput(inputId = "category",
      label = "Select category label",
      choices = c("All", "Gov", "Foursquare", "Jail", "News", "Sports",
          "Weather", "Junk"),
      selected = "All"),    
    checkboxInput("rt", "Show Retweets", FALSE),
    br(),
    dateRangeInput("daterange", "Date range:",
                   start = Sys.Date()-7,
                   end = Sys.Date()),
    br(),
    textInput("search.term", "Subset Data By Search Term", ""),
    br(),
    br(),
    tags$img(src="SMRTCHIC.png", width=250),
    br(),
    br(),
    HTML('We mine <a href="https://twitter.com/">Twitter</a> for mentions of 
         "Cook County". This dashboard represents what we have found. For more 
          information about this project, see our <a href = 
         "http://www.smartchicagocollaborative.org/">information page</a>.'),
    br(),
    br(),
    HTML('The code for this dashboard is located on <a href="https://github.com/corynissen/cook-county-tweet-dashboard">Github</a>'),
    tags$p("Verson 0.1")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Tweets",
        h3(textOutput("caption")),
        plotOutput("plot"),    
        uiOutput("tweet.table")
      ),
      tabPanel("Links",
        uiOutput(outputId = "links.freq.table"),
        uiOutput(outputId = "links.table")
        #tableOutput("links.table")
      ),
      tabPanel("Names",
        uiOutput(outputId = "names.freq.table"),
        uiOutput("names.table")
      )
    )
  )
))


library(shiny)
library(lubridate)

source("update_data.R")  # df is all the data
load("data.Rdata")

shinyServer(function(input, output) {

  data <- reactive({
    if(input$refresh > 0){
      source("update_data.R")
    }
    df$created_at3 <- gsub("\\+0000 ", "", df$created_at)
    df$created_at3 <- parse_date_time(substring(df$created_at3, 5, nchar(df$created_at3)), "%b %d %H:%M:%S %Y")
    df$epoch <- seconds(df$created_at3)
    df <- df[order(df$epoch, decreasing=TRUE),]
    if(!input$rt){
      df <- subset(df, !is.rt)
    }

    if(input$category != "All"){
      df <- subset(df, category==input$category)
    }
    
    df
  })
    
  output$caption <- renderText({
    "County Tweet Categories by Time"
  })

  output$plot <- renderPlot({
    df <- data()
    tmp <- as.data.frame(table(df$created_at2, df$category))
    tmp$Var1 <- as.character(tmp$Var1)
    tmp$Var2 <- as.character(tmp$Var2)    
    
    p <- ggplot(tmp) + geom_point(aes(x=Var1, y=Freq, colour=Var2)) +
         geom_line(aes(x=Var1, y=Freq, colour=Var2, group=Var2))
    print(p)
  })

  output$tweet.table <- renderTable({
      df <- data()
      tab <- subset(df, select=c("text", "category", "created_at"))      
  },include.rownames=FALSE)
      
})

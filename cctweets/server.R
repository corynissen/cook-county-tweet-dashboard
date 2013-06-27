
library(shiny)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
source("update_data.R") # df is all the data

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  data <- reactive({
    df <- df[order(df$created_at2, decreasing=TRUE),]
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

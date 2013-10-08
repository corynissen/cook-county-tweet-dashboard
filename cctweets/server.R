
library(shiny)
library(lubridate)
library(reshape2)
source('df2html.R')

shinyServer(function(input, output, session) {
  #source("update_data.R")  # df is all the data
  load("cookcounty.Rdata")  

  data <- reactive({    
    ## if(input$refresh > 0){
    ##   isolate({
    ##     source("update_data.R")
    ##     load("all_fp.Rdata")
    ##     df$is.rt <- as.logical(df$is.rt)
    ##     df$epoch <- as.numeric(df$epoch)
    ##     df$created.at3 <- as.Date(df$created.at2)
    ##     df$city <- city.key[match(df$search_term, city.key[,2]),1]
    ##   })
    ## }
    df <- df[order(df$epoch, decreasing=TRUE),]
    if(!input$rt){
      df <- subset(df, !is.rt)
    }

    if(input$category != "All"){
      df <- subset(df, category==input$category)
    }    
    if(input$search.term != ""){
      df <- subset(df, grepl(tolower(input$search.term), tolower(df$text)))
    }
    
    df
  })

  # had to subset the data after the slider was created
  subset.data <- reactive({
    df <- data()
    max.date <- max(df$created_at3)
    df <- subset(df, created_at3 >= max.date-days(input$day.slider.reactive))
    #if(!is.null(input$links.table)){
    #  if(input$links.table > 0){
    #    selected.link <- get.selected.link()
    #    if(selected.link != "NULL"){
    #      df <- subset(df, embedded.url.long.hostname.short==selected.link)
    #      input$links.table <- NULL
    #    }
    #  }
    #}
    df
  })

  get.links.freq.table <- reactive({
    df <- subset.data()
    tab <- table(df$embedded.url.long.hostname.short)
    links.df <- data.frame(hostname=names(tab), count=as.numeric(tab),
                           stringsAsFactors=F)
    links.df <- links.df[order(links.df$count, decreasing=T),]
    links.df <- subset(links.df, hostname!="")
    links.df
  })

  get.selected.link <- reactive({
    if(!is.null(input$links.freq.table)){
      if(input$links.freq.table > 0){
        links.df <- get.links.freq.table()
        ret <- links.df[input$links.freq.table,1]
      }else{
        ret <- "NULL"
      }
    }else{
      ret <- "NULL"
    }
    ret
  })

  # create the slider here because I need input from the df dataframe
  output$day.slider <- renderUI({
    df <- data()
    min.date <- min(df$created_at3)
    max.date <- max(df$created_at3)
    max.value <- ceiling(as.numeric((max.date - min.date)))
    return(sliderInput("day.slider.reactive", "Date range (back from present)",
                       min=1, max=max(c(2, max.value)), value=max(c(1,
                                                 min(c(7, max.value))))))
  })  

  output$caption <- renderText({
    "Cook County Tweets by Time"
  })

  output$plot <- renderPlot({
    df <- subset.data()
    #df$created.at3 <- substring(df$created.at2, 1,
    #                            regexpr(" ", df$created.at2)-1)
    tmp <- as.data.frame(table(df$created_at2, df$category))
    tmp$Var1 <- as.character(tmp$Var1)
    tmp$Var2 <- as.character(tmp$Var2)
    overall.len <- length(unique(tmp$Var1))
    ticks <- c(seq(1, overall.len, ceiling(overall.len / 10)), overall.len)
    
    p <- ggplot(tmp) + geom_point(aes(x=Var1, y=Freq, colour=Var2)) +
         geom_line(aes(x=Var1, y=Freq, colour=Var2, group=Var2)) +
         xlab("Date") + ylab("Count of Tweets") +
         scale_colour_discrete(name="Tweet Label") +
         scale_x_discrete(breaks=tmp[ticks, "Var1"])
    print(p)
  })

  output$tweet.table <- renderTable({
    df <- subset.data()
    tab <- subset(df, select=c("text", "category", "created.at",
                          "status.link"))
      
  },include.rownames=FALSE, sanitize.text.function = function(x) x)
 
  output$links.freq.table <- renderUI({
    links.df <- get.links.freq.table()
    HTML(df2html(links.df, class = "tbl selRow", id = "links.freq.table"))
  })

  output$links.table <- renderTable({
    df <- subset.data()
    selected.link <- get.selected.link()
    if(selected.link != "NULL"){
      df.filtered <- subset(df, embedded.url.long.hostname.short==selected.link)
    }else{
      df.filtered <- df
    }
    df.filtered$embedded.link <- paste0('<a href="',
                                      df.filtered$embedded.url.long,
                                      '" target="_blank">Follow Link</a>')
    tab <- subset(df.filtered, select=c("text", "created.at", "embedded.link"))  
  },include.rownames=FALSE, sanitize.text.function = function(x) x)

# debug stuff... remove eventually  
observe({print(paste0("Table 2: ", ifelse(is.null(input$links.freq.table), "NULL", input$links.freq.table)))})
observe({print(get.selected.link())})
  
})


#output$testTbl2 <- renderUI({
#    if (!is.null(testdf()))
#        HTML(df2html(testdf(), class = "tbl selRow", id = "testTbl2"))
#    })

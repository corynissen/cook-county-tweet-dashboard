
library(shiny)
library(lubridate)
library(reshape2)
source('df2html.R')

shinyServer(function(input, output, session) {
  load("cookcounty.Rdata")  

  ##########################################################################
  # Left panel stuff
  ##########################################################################
  data <- reactive({    
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
    
    daterange <- input$daterange
    start.date <- daterange[1]
    end.date <- daterange[2]
    start.date <- ifelse(start.date < min(df$created_at2), 
                         min(df$created_at2), start.date)
    start.date <- ifelse(start.date > max(df$created_at2), 
                         max(df$created_at2) - 7, start.date)
    end.date <- ifelse(end.date > max(df$created_at2), 
                         max(df$created_at2), end.date)
    df <- subset(df, created_at2 >= start.date &
                     created_at2 <= end.date)    
    df
  })
 
  ##########################################################################
  # Tweets tab stuff
  ##########################################################################
  output$caption <- renderText({
    "Cook County Tweets by Time"
  })

  output$plot <- renderPlot({
    df <- data()
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

  output$tweet.table <- renderUI({
    df <- data()
    tab <- subset(df, select=c("text.with.links", "category", "created.at4",
                          "status.link"))
    HTML(df2html(tab, class = "tbl", id = "tweet.table"))
  })
  
  ##########################################################################
  # Links tab stuff
  ##########################################################################
  get.links.freq.table <- reactive({
    df <- data()
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

  output$links.freq.table <- renderUI({
    links.df <- get.links.freq.table()
    HTML(df2html(links.df, class = "tbl selRow links_freq_table",
                 id = "links.freq.table"))
  })

  output$links.table <- renderUI({
    df <- data()
    selected.link <- get.selected.link()
    if(selected.link != "NULL"){
      df.filtered <- subset(df, embedded.url.long.hostname.short==selected.link)
    }else{
      df.filtered <- df
    }
    df.filtered$embedded.link <- paste0('<a href="',
                                      df.filtered$embedded.url.long,
                                      '" target="_blank">Follow Link</a>')
    tab <- subset(df.filtered, select=c("text.with.links", "created.at4",
                                   "status.link"))
    HTML(df2html(tab, class = "tbl links_table", id = "links.table"))
  })

  ##########################################################################
  # Names tab stuff
  ##########################################################################
  get.names.freq.table <- reactive({
    df <- data()
    tab <- table(unlist(strsplit(df$people.names, " \\| ")))
    names.df <- data.frame(name=names(tab), count=as.numeric(tab),
                           stringsAsFactors=F)
    names.df <- names.df[order(names.df$count, decreasing=T),]
    names.df
  })

  get.selected.name <- reactive({
    if(!is.null(input$names.freq.table)){
      if(input$names.freq.table > 0){
        names.df <- get.names.freq.table()
        ret <- names.df[input$names.freq.table,1]
      }else{
        ret <- "NULL"
      }
    }else{
      ret <- "NULL"
    }
    ret
  })

  output$names.freq.table <- renderUI({
    names.df <- get.names.freq.table()
    HTML(df2html(names.df, class = "tbl selRow", id = "names.freq.table"))
  })

  output$names.table <- renderUI({
    df <- data()
    selected.name <- get.selected.name()
    if(selected.name != "NULL"){
      df.filtered <- subset(df, grepl(selected.name, df$text))
    }else{
      df.filtered <- df
    }    
    tab <- subset(df.filtered, select=c("text.with.links", "created.at4", 
                                        "status.link"))
    HTML(df2html(tab, class = "tbl names_table", id = "names.table"))
  })

# debug stuff... remove eventually  
#observe({print(paste0("Table 2: ", ifelse(is.null(input$links.freq.table), "NULL", input$links.freq.table)))})
observe({print(input$daterange)})  
observe({print(str(input$daterange))})  
  
})

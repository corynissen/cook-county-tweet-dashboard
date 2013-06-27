
library(RMongo)
library(lubridate)
library(textcat)
library(stringr)
load("c_model.Rdata")

update.df <- function(dataframe){
  max.date <- max(dataframe$epoch, na.rm=T) * 1000
  new.df <-  dbGetQuery(mongo, "tweets", query=paste0('{"jDate":{"$gt":{"$date":', max.date, '}}}'), 0, 1000000)  
  if(nrow(new.df) > 0){
    new.df <- do.model(new.df)
    add_these_cols <- names(dataframe)[!names(dataframe) %in% names(new.df)]
    for(col in add_these_cols){
      new.df[,col] <- rep(NA, nrow(new.df))
    }
    new.df <- as.data.frame(rbind(dataframe, new.df))
  }else{
    new.df <- dataframe
  }
  return(new.df)
}

do.model <- function(dataframe){
  dataframe$text <- iconv(dataframe$text, "", "UTF-8", sub="")
  dataframe$text.cleansed <- as.character(sapply(dataframe$text, function(x)clean.text(x)))
  dataframe$created_at2 <- as.Date(dataframe$created_at, "%a %b %d %H:%M:%S +0000 %Y")
  dataframe$is.rt <- grepl("^RT| RT @", dataframe$text)
  dataframe$created_at3 <- gsub("\\+0000 ", "", dataframe$created_at)
  dataframe$created_at3 <- parse_date_time(substring(dataframe$created_at3, 5, nchar(dataframe$created_at3)), "%b %d %H:%M:%S %Y")
  dataframe$epoch <- seconds(dataframe$created_at3)
  dataframe$category <- textcat(dataframe$text.cleansed, c.model)
  news.phrases <- c("Cook County News:", "via @crainschicago", "PRESS RELEASE:")
  dataframe$category[grepl(paste(news.phrases, collapse="|"), dataframe$text, ignore.case=TRUE)] <- "News"
  return(dataframe)
}

# must have ssh tunnel up and running
mongo <- mongoDbConnect("cctweets")
source("pw.R")
authenticated <- dbAuthenticate(mongo, username, password)

if(file.exists("data.Rdata")){
  load("data.Rdata")
  df <- update.df(df)
}else{
  # just get the whole data set
  df <- dbGetQuery(mongo, "tweets", '{}', 0, 1000000)
  df <- do.model(df)
}

save(list=c("df"), file="data.Rdata")

# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongo_tunnel.sh ubuntu@54.227.2.128:/src/keys
# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongodb1.pem ubuntu@54.227.2.128:/src/keys

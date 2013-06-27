
library(RMongo)
library(lubridate)
library(textcat)
library(stringr)
load("c_model.Rdata")

myURLdecode <- function(URL)
{
    x <- charToRaw(URL)
    pc <- charToRaw("%")
    out <- raw(0L)
    i <- 1L
    len <- length(x)
    while(i <= len) {
        if(x[i] != pc || i + 2 > len) {
            out <- c(out, x[i])
            i <- i + 1L
        } else {
            y <- as.integer(x[i + 1L:2L])
            y[y > 96L] <- y[y > 96L] - 32L # a-f -> A-F
            y[y > 57L] <- y[y > 57L] - 7L  # A-F
            if (y[1] > 48L) {
                y <- sum((y - 48L) * c(16L, 1L))
                out <- c(out, as.raw(as.character(y)))
                i <- i + 3L
            } else {
                out <- c(out, pc)
                i <- i + 1L
            }
        }
    }
    rawToChar(out)
}

update.df <- function(dataframe){
  created_at2 <- gsub("\\+0000 ", "", dataframe$created_at)
  created_at2 <- parse_date_time(substring(created_at2, 5, nchar(created_at2)), "%b %d %H:%M:%S %Y")
  epoch <- seconds(created_at2)
  max.date <- dataframe$created_at[which.max(epoch)]
  new.df <- dbGetQuery(mongo, "tweets", paste0('{"created_at": {"$gt": "', max.date, '"}}'), 0, 1000000)
  new.df <- do.model(new.df)
  new.df <- as.data.frame(rbind(dataframe, new.df))
  return(new.df)
}

do.model <- function(dataframe){
  dataframe$text <- iconv(dataframe$text, "", "latin1", sub="")
  dataframe$text <- as.character(sapply(dataframe$text, myURLdecode))
  dataframe$text.cleansed <- as.character(sapply(dataframe$text, function(x)clean.text(x)))
  dataframe$created_at2 <- as.Date(dataframe$created_at, "%a %b %d %H:%M:%S +0000 %Y")
  dataframe$is.rt <- grepl("^RT| RT @", dataframe$text)
  dataframe$category <- textcat(dataframe$text.cleansed, c.model)
  news.phrases <- c("Cook County News:", "via @crainschicago", "PRESS RELEASE:")
  dataframe$category[grepl(paste(news.phrases, collapse="|"), dataframe$text, ignore.case=TRUE)] <- "News"
  return(dataframe)
}

system("sh /src/keys/mongo_tunnel.sh &")
mongo <- mongoDbConnect("cctweets")
username = "cctweets" 
password = "cctweets" 
authenticated <- dbAuthenticate(mongo, username, password)

if(file.exists("data.Rdata")){
  load("data.Rdata")
  df <- update.df(df)
}else{
  # just get the whole data set
  df <- dbGetQuery(mongo, "tweets", '{}', 0, 1000000)
  df <- do.model(df)
}

system("killall ssh")

save(list=c("df"), file="data.Rdata")

# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongo_tunnel.sh ubuntu@54.227.2.128:/src/keys
# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongodb1.pem ubuntu@54.227.2.128:/src/keys

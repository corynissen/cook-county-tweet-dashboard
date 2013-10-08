
library(lubridate)
library(RJSONIO)
library(RCurl)
library(stringr)
library(textcat)
library(parallel)
library(httr)
load("c_model.Rdata")

tablename <- "cory_tweets"
searchterm <- "cook_county"
max.id <- "0"

update.df <- function(df, tablename, searchterm){
  max.id <- max(df$tweetid[df$search_term==searchterm])
  new.df <- read.tweets(tablename, searchterm, max.id)
  if(nrow(new.df) > 0){
    new.df <- add.cols(new.df)
    new.df <- data.frame(rbind(df, new.df), stringsAsFactors=F)
  }else{
    new.df <- df
  }
  return(new.df)
}

add.cols <- function(df){
  df$text <- iconv(df$text, "", "UTF-8", sub="")
  df$text.cleansed <- as.character(sapply(df$text, function(x)clean.text(x)))
  df$created_at2 <- as.Date(df$created.at, "%a %b %d %H:%M:%S +0000 %Y")
  df$is.rt <- grepl("^RT| RT @", df$text)
  df$created_at3 <- gsub("\\+0000 ", "", df$created.at)
  df$created_at3 <- parse_date_time(substring(df$created_at3, 5,
                      nchar(df$created_at3)), "%b %d %H:%M:%S %Y")
  df$epoch <- seconds(df$created_at3)
  df$category <- textcat(df$text.cleansed, c.model)
  news.phrases <- c("Cook County News:", "via @crainschicago", "PRESS RELEASE:")
  weather.phrases <- c("Severe Thunderstorm", "Severe t-storm", "flash flood",
                       "storm warning", "weather alert")
  foursq.phrases <- c("I'm at")
  df$category[grepl(paste(news.phrases, collapse="|"), df$text,
                    ignore.case=TRUE)] <- "News"
  df$category[grepl(paste(weather.phrases, collapse="|"), df$text,
                    ignore.case=TRUE)] <- "Weather"
  df$category[grepl(paste(foursq.phrases, collapse="|"), df$text,
                    ignore.case=TRUE)] <- "Foursquare"
  df$status.link <- paste0('<a href="https://twitter.com/', df$author,
                           '/status/', df$tweetid,
                           '" target="_blank">View on Twitter</a>')
  df$embedded.url <- str_extract(df$text,
                       "http://[A-Za-z0-9].[A-Za-z]{2,3}/[A-Za-z0-9]+")
  df$embedded.url.long <- sapply(df$embedded.url,
                             function(x)ifelse(is.na(x), NA, getLongURL.api(x)))
  df$embedded.url.long.hostname <- sapply(df$embedded.url.long,
                                          function(x)ifelse(!is.na(x) & x!="",
                                            parse_url(x)$hostname, ""))
  df$embedded.url.long.hostname <- gsub("www.", "",
                                        df$embedded.url.long.hostname)
  df$embedded.url.long.hostname.short <- substring(df$embedded.url.long.hostname,
                                           regexpr("[a-zA-Z0-9]+.[a-zA-Z]+$",
                                           df$embedded.url.long.hostname),
                                           nchar(df$embedded.url.long.hostname))
  return(df)
}

clean.text <- function(text){
  # INPUT: Text to be "cleansed"
  # OUTPUT: Cleansed text
  # USAGE: clean.text(text) will return a string that has the punctuation removed
  #        lower case, and all other text cleaning operations done
  replace.links <- function(text){
    # extract urls from string, only works with t.co links, which all links in
    # twitter are nowadays
    return(str_replace_all(text,
                           ignore.case("http://[a-z0-9].[a-z]{2,3}/[a-z0-9]+"),
                           "urlextracted"))
  }
  remove.word <- function(string, starts.with.char){
    # INPUT:  string is a string to be edited,
    #         starts.with.char is a string or partial string to search and remove
    # OUTPUT: string with words removed
    # USAGE:  remove.word(string, "@") removes words starting with "@"
    #         remove.word(string, "RT") removes RT from string
    word.len <- nchar(starts.with.char)
    list.of.words <- strsplit(string, " ")[[1]]
    # remove ones that start with "starts.with.char"
    list.of.words <- list.of.words[!substring(list.of.words, 1,
                                              word.len)==starts.with.char]
    ret.string <- paste(list.of.words, collapse=" ")
    return(ret.string)
  }
  
  text.cleansed <- tolower(text)
  # remove the string "food poisoning" because every tweet has this in it...
  text.cleansed <- gsub("food poisoning", "", text.cleansed)
  text.cleansed <- replace.links(text.cleansed)
  text.cleansed <- remove.word(text.cleansed, "@")
  text.cleansed <- remove.word(text.cleansed, "rt")
  # replace non-letters with spaces
  text.cleansed <- gsub("[^[:alnum:]]", " ", text.cleansed)
  # remove leading and trailing spaces
  text.cleansed <- gsub("^\\s+|\\s+$", "", text.cleansed)
  # replace multiple spaces next to each other with single space
  text.cleansed <- gsub("\\s{2,}", " ", text.cleansed)
  return(text.cleansed)
}  

read.tweets <- function(tablename, searchterm, max.id){
  # throws warning is json is too long
  if(max.id==""){max.id <- "0"}
  tweets.json <-suppressWarnings(system(paste0(
      "python get_tweets_since_tweetid_from_dynamo.py '", tablename, "' '",
      searchterm, "' '", max.id, "'"), intern=T))
  tweets.json <- paste(tweets.json, collapse="")
  #tweets.json <- gsub('"', '\\\\"', tweets.json)
  tweets <- fromJSON(tweets.json)
  text <- sapply(tweets$tweets, "[[", "text")
  tweetid <- sapply(tweets$tweets, "[[", "tweetid")
  author <- sapply(tweets$tweets, "[[", "author")
  search_term <- sapply(tweets$tweets, "[[", "search_term")
  timestamp_pretty <- sapply(tweets$tweets, "[[", "timestamp_pretty")
  df <- as.data.frame(cbind(text=text, author=author, search_term=search_term,
                            tweetid=tweetid, created.at=timestamp_pretty),
                      stringsAsFactors=F)
  return(df)
}

getLongURL.curl <- function(shorturl){
  # uses curl statement to get expanded url from t.co links (or any link)
  # loop through until there's no location attribute... that's the long link.
  newurl <- shorturl
  url <- ""
  while(url != newurl){
    data <- system(paste0("curl -I --connect-timeout 5 ", newurl), intern=T)    
    if(sum(grepl("^location: ", tolower(data))) == 0 |
       sum(grepl("^http/1.1 302 moved temporarily", tolower(data)))>0){        
      url <- newurl
    }else{
      data <- subset(data, tolower(substring(data, 1, 9)) %in% c("location:"))
      stringurl <- substring(data[1], regexpr(":", data[1])+2, nchar(data[1])-1)
      # sometimes the location data isn't a url.
      if(substring(stringurl, 1, 4)=="http"){ 
        newurl <- stringurl
      }else{
        url <- newurl
      }
    }
  }
  return(newurl)
}
getLongURL.api <- function(shorturl){
  response <- getURL(paste0("http://api.longurl.org/v2/expand?url=", shorturl))
  # parse out long url
  if(grepl("<long-url", response)){
    longurl <- substring(response,
                         regexpr("<long-url><!\\[CDATA\\[", response)[1]+19,
                         regexpr("\\]\\]><\\/long-url>", response)[1]-1)
  }else{
    longurl <- ""
  }
  return(longurl)
}

  

if(file.exists("cookcounty.Rdata")){
  load("cookcounty.Rdata")
  df <- update.df(df, tablename, searchterm)
}else{
  df <- read.tweets(tablename, searchterm, "0")
  df <- add.cols(df)

}

df <- df[!duplicated(df$tweetid),]
save(list=c("df"), file="cookcounty.Rdata")


# scp -i ~/cn/chicago/keys/rserver.pem .chireply_twitter_creds ubuntu@107.22.187.183:/src/minneapolis-fp-tweets/minneapolis-fp-tweets

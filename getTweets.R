
library(RJSONIO)

read.tweets <- function(tablename, searchterm, max.id){
  # throws warning is json is too long
  if(max.id==""){max.id <- "0"}
  tweets.json <-suppressWarnings(system(paste0(
      "python cctweets/get_tweets_since_tweetid_from_dynamo.py '",
      tablename, "' '",
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

tablename <- "cory_tweets"
searchterm <- "cook_county"
max.id <- "0"

df.new <- read.tweets(tablename, searchterm, max.id)
df.old <- read.csv("county_tweets.csv", stringsAsFactors=F)

df.new <- subset(df.new, select=c("tweetid", "text"))
df.new$manual_class <- rep("", nrow(df.new))

df <- rbind(df.old, df.new)

write.csv(df, "county_tweets.csv", row.names=F)

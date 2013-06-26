
library(textcat)
library(stringr)

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

# read the manually classified tweets
df <- read.csv("county_tweets_062413.csv", stringsAsFactors=F)

# do the preprocessing to the data. this needs to be done before any prediction
# using the model that is created.
df$text.cleansed <- as.character(sapply(df$text, function(x)clean.text(x)))
df$created_at2 <- as.Date(df$created_at, "%a %b %d %H:%M:%S +0000 %Y")
df$is.rt <- grepl("^RT| RT @", df$text)

# split into cat and uncat data sets
df.uncat <- subset(df, is.na(manual_class))
df.cat <- subset(df, !is.na(manual_class) & text.cleansed != "")
df.cat <- subset(df.cat, !duplicated(text.cleansed))

# train the model using the textcat package
c.model <- textcat_profile_db(df.cat$text.cleansed, df.cat$manual_class)
# save model file to be used on server 
save(list=c("c.model", "clean.text"), file="c_model.Rdata")

# test it out...
# textcat("i have the food poisoning", fp.model)
#test <- sapply(df2$text.cleansed[1:100], function(x)textcat(x, c.model))
#paste0(as.character(test), " |||| ", df2$text[1:100])


df.uncat$pred <- textcat(df.uncat$text.cleansed, c.model)

df$category <- textcat(df$text.cleansed, c.model)
df$category[!is.na(df$manual_class)] <- df$manual_class[!is.na(df$manual_class)]

news.phrases <- c("Cook County News:", "via @crainschicago", "PRESS RELEASE:")
df$category[grepl(paste(news.phrases, collapse="|"), df$text, ignore.case=TRUE)] <- "News"

save(list=c("df.cat", "df.uncat", "df"), file="data.Rdata")


             

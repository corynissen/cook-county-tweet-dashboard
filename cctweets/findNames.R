
library(plyr)
library(parallel)

# read in 1990 census most popular first names
first.male <- read.table("http://www.census.gov/genealogy/www/data/1990surnames/dist.male.first", stringsAsFactors=F)
names(first.male) <- c("name", "perc", "cum.perc", "rank")
first.female <- read.table("http://www.census.gov/genealogy/www/data/1990surnames/dist.female.first", stringsAsFactors=F)
names(first.female) <- c("name", "perc", "cum.perc", "rank")

last.names <- read.table("http://www.census.gov/genealogy/www/data/1990surnames/dist.all.last", stringsAsFactors=F)
names(last.names) <- c("name", "perc", "cum.perc", "rank")

exceptions.first <- c("in", "aide", "my", "so", "august", "an", "karma", "chi",
                      "september", "long")
exceptions.last <- c("dick", "stay", "in", "bail", "be", "he", "to", "has", "them", "him", "co", "blue")

first.male <- subset(first.male, !tolower(name) %in% exceptions.first)
first.female <- subset(first.female, !tolower(name) %in% exceptions.first)
last.names <- subset(last.names, !tolower(name) %in% exceptions.last)

first.names <- c(first.male$name, first.female$name)

bigrams <- function(text){
  word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec.length <- length(word.vec)
  lapply(1:(word.vec.length-1), function(x)c(word.vec[x], word.vec[x+1]))
}

get.people.names <- function(text){
  text <- gsub("'s", "", text)
  text.split <- strsplit(text, " ")[[1]]
  text <- paste(gsub("[^a-zA-Z]", "", text.split), collapse=" ")
  text <- gsub("[[:punct:]]", "notaword", text)  # does nothing... change me!!!
  bigrams.list <- bigrams(text)
  bigrams.list <- lapply(bigrams.list, function(x)gsub("[^a-zA-Z]", "", x))
  bigrams.list <- bigrams.list[lapply(bigrams.list,
                                      function(x)sum(grepl("notaword", x))) < 1]
  bigrams.list <- bigrams.list[lapply(bigrams.list,
                                      function(x)sum(grepl("^[A-Z]", x))) == 2]
  is.name <- function(bigram){
    has.last.name <- tolower(bigram[2]) %in% tolower(last.names$name)
    if(has.last.name){      
      has.first.name <- tolower(bigram[1]) %in% tolower(first.names)
      isname <- has.first.name & has.last.name
    }else{
      isname <- has.last.name
    }
    return(isname)
  }
  if(length(bigrams.list) > 0){
    name.indexes <- sapply(bigrams.list, is.name)
    bigrams.list <- bigrams.list[name.indexes]
    name.vec <- sapply(bigrams.list, function(x)paste(x, collapse=" "))
  }else{
    name.vec <- ""
  }
  return(paste(name.vec, collapse=" | "))  
}

#people.name <- sapply(mclapply(df$text, get.people.names, mc.cores=3),
#                          function(x)paste(x, collapse=", "))

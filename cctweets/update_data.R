
library(RMongo)
library(lubridate)

update.df <- function(dataframe){
  created_at2 <- gsub("\\+0000 ", "", dataframe$created_at)
  created_at2 <- parse_date_time(substring(created_at2, 5, nchar(created_at2)), "%b %d %H:%M:%S %Y")
  epoch <- seconds(created_at2)
  max.date <- dataframe$created_at[which.max(epoch)]
  new.df <- dbGetQuery(mongo, "tweets", paste0('{"created_at": {"$gt": "', max.date, '"}}'), 0, 1000000)
  new.df <- as.data.frame(rbind(dataframe, new.df))
  return(new.df)
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
}

save(list=c("df"), file="data.Rdata")

# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongo_tunnel.sh ubuntu@54.227.2.128:/src/keys
# scp -i ~/cn/chicago/keys/rserver.pem ~/cn/chicago/keys/mongodb1.pem ubuntu@54.227.2.128:/src/keys

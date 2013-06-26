
library(RMongo)

system("sh ~/cn/chicago/keys/mongo_tunnel.sh")
mongo <- mongoDbConnect("cctweets")
username = "username here"
password = "password here"
authenticated <- dbAuthenticate(mongo, username, password)
dbShowCollections(mongo)
df <- dbGetQuery(mongo, "tweets", "{}", 0, 1000000)
df <- subset(df, X_id!="")
df$manual_class <- rep(NA, nrow(df))
write.csv(df, "county_tweets_062413.csv", row.names=F)


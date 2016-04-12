require(plyr)
detach("package:RMySQL", unload = TRUE)
detach("package:sqldf", unload = TRUE)
require(sqldf)

load("C:/Users/steve/Dropbox/Tweets/tweets_3.2_to_4.2.Rdata")
setwd("C:/Users/steve/Documents/QMSS Program/Data Visualization/Final Project")

write.csv(df_all[ ,c('id_str', 'place_lat', 'place_lon')], "march_tweets.csv")
###data mapped in QGIS
march_tweets_mapped <- read.csv("march_tweets_mapped.csv")
###Primary Info scrapped from realclearpoltics.com
primary_info <- read.csv("primary_info.csv")

###merging tweets with states
march_tweets_states <- merge(df_all, march_tweets_mapped[ ,c("id_str", "STATEFP", "NAME")], by = "id_str")
###merging tweet sentiment scores and candidate flags
sentiments <- read.csv("sentiment.draft.csv", header = T, stringsAsFactors = F)
march_tweets_all <- merge(march_tweets_states, sentiments, by = "id_str")

###republican primary table
republican_primary_table <- subset(primary_info, Party == 'Republican')
rep_candidates <- c("Trump", "Cruz", "Rubio", "Kasich")
rep_primary_tabulation_table <- merge(republican_primary_table, rep_candidates)

###democrat primary table
democrat_primary_table <- subset(primary_info, Party == 'Democrat')
dem_candidates <- c("Clinton", "Sanders")
dem_primary_tabulation_table <- merge(democrat_primary_table, dem_candidates)

###getting date in date format
march_tweets_all$date <- as.Date(substr(march_tweets_all$created_at, 5, 10), "%B %d" )
head(march_tweets_all$date)
rep_primary_tabulation_table$primary_date <-  as.Date(rep_primary_tabulation_table$Date, "%m/%d/%Y")
republican_primary_table$primary_date <-  as.Date(republican_primary_table$Date, "%m/%d/%Y")
republican_primary_table
march_tweets_all <- rename(march_tweets_all, c("NAME" = "state_name"))


##summarizing tweets
model_file <- sqldf("select a.*, 

      count (case when a.State = b.state_name and a.primary_date > b.date and b.trump_tweet = 1  
      then b.id_str end) as trump_tweets,
      count (case when a.State = b.state_name and a.primary_date > b.date and b.rubio_tweet = 1  
      then b.id_str end) as rubio_tweets,
      count (case when a.State = b.state_name and a.primary_date > b.date and b.cruz_tweet = 1  
      then b.id_str end) as cruz_tweets,
      count (case when a.State = b.state_name and a.primary_date > b.date and b.kasich_tweet = 1  
      then b.id_str end) as kasich_tweets,

      count (case when a.State = b.state_name and raw_score > 0 and a.primary_date > b.date and b.trump_tweet = 1  
      then b.id_str end) as trump_pos_tweets,
      count (case when a.State = b.state_name and raw_score > 0 and a.primary_date > b.date and b.rubio_tweet = 1  
      then b.id_str end) as rubio_pos_tweets,
      count (case when a.State = b.state_name and raw_score > 0 and a.primary_date > b.date and b.cruz_tweet = 1  
      then b.id_str end) as cruz_pos_tweets,
      count (case when a.State = b.state_name and raw_score > 0 and a.primary_date > b.date and b.kasich_tweet = 1  
      then b.id_str end) as kasich_pos_tweets

      
      from republican_primary_table a
      left join march_tweets_all b
      group by Party, State, a.Date, Winner, Date_Month, primary_date
      ")


##calculating proportions metrics

model_file$tweet_count <- sum(model_file$trump_tweets, model_file$rubio_tweets, model_file$cruz_tweets, model_file$kasich_tweets)
model_file$trump_prop <- model_file$trump_tweets / model_file$tweet_count
model_file$rubio_prop <- model_file$rubio_tweets / model_file$tweet_count
model_file$cruz_prop <- model_file$cruz_tweets / model_file$tweet_count
model_file$kasich_prop <- model_file$kasich_tweets / model_file$tweet_count

# model_file$pos_tweet_count <- sum(model_file$trump_pos_tweets, model_file$rubio_pos_tweets, model_file$cruz_pos_tweets, model_file$kasich_pos_tweets)
# model_file$trump_pos_prop <- model_file$trump_pos_tweets / model_file$pos_tweet_count
# model_file$rubio_pos_prop <- model_file$rubio_pos_tweets / model_file$pos_tweet_count
# model_file$cruz_pos_prop <- model_file$cruz_pos_tweets / model_file$pos_tweet_count
# model_file$kasich_pos_prop <- model_file$kasich_pos_tweets / model_file$pos_tweet_count

model_file$trump_pos_prop <- model_file$trump_pos_tweets / model_file$trump_tweets
model_file$rubio_pos_prop <- model_file$rubio_pos_tweets / model_file$rubio_tweets
model_file$cruz_pos_prop <- model_file$cruz_pos_tweets / model_file$cruz_tweets
model_file$kasich_pos_prop <- model_file$kasich_pos_tweets / model_file$kasich_tweets


require(VGAM)
require("nnet")

#first test models
rep_prediction_model <- multinom(Winner ~ trump_prop*trump_pos_prop +
                                   rubio_prop*rubio_pos_prop +
                                   cruz_prop*cruz_pos_prop + 
                                   kasich_prop*kasich_pos_prop
                                
                                  ,
                                 data = subset(model_file, tweet_count > 0 & Party == 'Republican'))

summary(rep_prediction_model)

primary_predictions <- cbind(republican_primary_table, predict(rep_prediction_model, model_file, type = "probs"))
primary_predictions







# table(substr(df_all$created_at, 8 ,10 ))

# 
# (0.25 / 7500) ** 0.5 * 1.98
# 
# moe_7500_cd <- (0.25 / (7500 / 435)) ** 0.5 * 1.98
# moe_7500_cd
# moe_7500_cd 
# read.csv()

# table(df_all$full_name, useNA = 'ifany')
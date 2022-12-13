#Cleaning
setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries

library(dplyr)
library(readr)
library(summarytools)
library(data.table)
library(lfe)
library(car)   # Install anytime package
library(lubridate) 

#load & inspect data
giveaways <- read.csv("Data/giveaways_thesis.csv")
ratings <- fread("Data/reviews_thesis_notext.csv")
giveaway <- fread("Data/df_giveaway.csv")

#check for matches remove no match
ratings$match <- ifelse(ratings$book_id %in% giveaway$book_id, 1,0)
ratings <- subset(ratings, match >= 1)

#merging
ratings <- merge(x = ratings, y = giveaway[ , c("book_id", "winning_odds", "booktype" )], by = "book_id", all.x=TRUE)
ratings <- merge(x = ratings, y = giveaways[, c("book_id", "giveaway_end_date")], by = "book_id", all.x = TRUE)

#conversion before-after
ratings$time <- mdy(ratings$time)
ratings$giveaway_end_date <- mdy(ratings$giveaway_end_date)
ratings$giveaway <- ifelse(ratings$time > ratings$giveaway_end_date, 1, 0)
ratings$timeym <- substr(ratings$time, 0,7)
ratings$end_dateym <- substr(ratings$giveaway_end_date,0,7)
ratings$volume <- ifelse(ratings$timeym > ratings$end_dateym, 1, 0 )
View(ratings)

# removing outliers
ratings <- subset(ratings, timeym > "2006-11")
ratings <- subset(ratings, end_dateym  < "2021-12")

#volume before after
df3 <- ratings %>% group_by(book_id,timeym, volume, booktype, winning_odds, giveaway) %>% 
  summarise(volume=n(),.groups = 'drop') %>% 
  as.data.frame()
View(df3)


#write csv
write.csv(ratings, 'Data/ratings.csv', row.names = F)
write.csv(df3, 'Data/volume_df.csv', row.names = F)

dfSummary(ratings$giveaway)


#Cleaning
setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries

library(dplyr)
library(readr)
library(summarytools)
library(data.table)
library(car)
library(lubridate) 

#load & inspect data
giveaways <- fread("Data/giveaways_thesis.csv")
book_publisher <- fread("Data/book_df_cleanPublisher.csv")
ratings <- fread("Data/reviews_thesis_notext.csv")

#remove unnecessary columns from book_publisher & rename column
booklist <- (book_publisher[-c(31:63)])
names(booklist)[names(booklist) == "id"] <- "book_id"

#find duplicates & weird cases
booklist <- add_count(booklist,book_id, name = "duplicate") %>% arrange(desc(duplicate))
giveaways <-add_count(giveaways,book_id, name = "duplicate") %>% arrange(desc(duplicate))
booklist$weird <- ifelse(booklist$average_rating > 0 & booklist$ratings_count == 0 & booklist$text_reviews_count == 0, 1,0)

# 2 duplicates for booklist, 0 for giveaways + weird cases removed
booklist <- subset(booklist, duplicate < 2 & weird == 0)

#removeformat is blank
booklist <- subset(booklist, format != "" & format != "Unknown Binding")

dfSummary(booklist$format)
#combine giveaways & book list via book ID (1 if giveaway, 0 else)
booklist$giveaway <- ifelse(booklist$book_id %in% giveaways$book_id, 1,0)


#change copy type to dummy (2 ebook, 1 hard copy, 3 other)
booklist <- booklist %>% 
  mutate(booktype = case_when(format == "Hardcover" ~ 1,
                              format == "Paperback" ~ 1,
                              format == "Mass Market Paperback" ~ 1,
                              format == "Spiral-bound" ~ 1,
                              format == "Trade Paperback" ~ 1,
                              format == "Board Book" ~ 1,
                              format == "Kindle Edition" ~ 2,
                              format == "ebook" ~ 2,
                              TRUE   ~ 3))

#add winning_odds to booklist & remove n<10
booklist$n_copy <- ifelse(booklist$book_id %in% giveaways$book_id, giveaways$copy_n, 0)
booklist$n_participants <- ifelse(booklist$book_id %in% giveaways$book_id, giveaways$request_n, 0)
booklist$winning_odds <-(booklist$n_copy/booklist$n_participants) 


#remove inf as it shifts the mean and they have no participants
# also only use data from giveaway subset
giveawaylist <- subset(booklist, winning_odds != 'Inf', & giveaway ==1)
giveaway <- giveawaylist[,c(1,3,12,13,19,23,24,33,67,68,69,70)]
write.csv(giveaway, 'Data/df_giveaway.csv', row.names = F)


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
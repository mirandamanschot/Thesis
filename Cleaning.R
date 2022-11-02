#Cleaning
setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries
library(dplyr)
library(readr)
#load & inspect data
giveaways <- read.csv("Data/giveaways_thesis.csv")
author_df <- read.csv("Data/author_df.csv")
reviews_notext <- read.csv("Data/reviews_thesis_notext.csv")
reviews_text <- read.csv("Data/reviews_thesis_text.csv")
book_publisher <- read.csv("Data/book_df_cleanPublisher.csv")

#remove unnecessary columns from book_publisher & rename column
booklist <- (book_publisher[-c(31:63)])
names(booklist)[names(booklist) == "id"] <- "book_id"

#find duplicates & weird cases
booklist <- add_count(booklist,book_id, name = "duplicate") %>% arrange(desc(duplicate))
giveaways <-add_count(giveaways,book_id, name = "duplicate") %>% arrange(desc(duplicate))
booklist$weird <- ifelse(booklist$average_rating > 0 & booklist$ratings_count == 0 & booklist$text_reviews_count == 0, 1,0)

# 2 duplicates for booklist, 0 for giveaways + weird cases removed
booklist <- subset(booklist, duplicate < 2 & weird == 0)

#combine giveaways & book list via book ID (1 if giveaway, 0 else)
booklist$giveaway <- ifelse(booklist$book_id %in% giveaways$book_id, 1,0)

#change copy type to dummy (0 ebook, 1 hard copy)
booklist$booktype_dummy <-ifelse(booklist$is_ebook== 'False',1,0)

#add winning_odds to booklist

booklist$n_copy <- ifelse(booklist$book_id %in% giveaways$book_id, giveaways$copy_n, 0)
booklist$n_participants <- ifelse(booklist$book_id %in% giveaways$book_id, giveaways$request_n, 0)
booklist$winning_odds <-(booklist$n_copy/booklist$n_participants)

#remove inf as it shifts the mean and they have no participants
booklist <- subset(booklist, winning_odds < 'inf')

#save file
write.csv(booklist, 'Data/booklist.csv', row.names = F)





#Cleaning
setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries

library(dplyr)
library(readr)
library(summarytools)
library(data.table)
#load & inspect data
giveaways <- read.csv("Data/giveaways_thesis.csv")
book_publisher <- read.csv("Data/book_df_cleanPublisher.csv")
similar_meta_thesis <- read.csv("Data/similar_meta_thesis.csv")

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
giveawaylist <- subset(booklist, winning_odds != 'Inf' & giveaway ==1)

#remove overlap similar_meta & booklist as those similar books did participate in a giveaway
similar_meta_thesis$overlap <- ifelse(similar_meta_thesis$similar_book_id %in% booklist$book_id, 1,0)
similar_books <- subset(similar_meta_thesis, overlap <1)

#change bookFormat to 1 = physical, 2 = ebook, 3 = other
dfSummary(similar_books$bookFormat)
similar_books <- subset(similar_books, bookFormat != "" & bookFormat != "Unknown Binding")

similar_books <- similar_books %>% 
  mutate(booktype = case_when(bookFormat == "Hardcover" ~ 1,
                              bookFormat == "Paperback" ~ 1,
                              bookFormat == "Mass Market Paperback" ~ 1,
                              bookFormat == "Spiral-bound" ~ 1,
                              bookFormat == "Trade Paperback" ~ 1,
                              bookFormat == "Board Book" ~ 1,
                              bookFormat == "Kindle Edition" ~ 2,
                              bookFormat == "ebook" ~ 2,
                              TRUE   ~ 3))

#prep to merge
df1 <- giveawaylist[c(1,3,12,13,19,23,24,33,34,37)]
df2 <- similar_books[c(1,2,4,5,6,18,19,23)]

df1<- df1 %>%
  mutate(year_month = with(., sprintf("%d-%02d", publication_year, publication_month)))

df2 <- df2 %>% 
  mutate(publication_month = case_when(
    publication_month =="january" ~ 1,
    publication_month == "february" ~ 2,
    publication_month == "march" ~ 3,
    publication_month == "april" ~ 4,
    publication_month == "may" ~ 5,
    publication_month == "june" ~ 6,
    publication_month == "july" ~ 7,
    publication_month == "august" ~ 8,
    publication_month == "september"~ 9,
    publication_month == "october" ~ 10,
    publication_month == "november" ~ 11,
    publication_month == "december" ~ 12,
    TRUE ~ 0))
df2 <- df2 %>%
  mutate(year_month = with(., sprintf("%d-%s", publication_year, as.numeric(publication_month))))
df2$publication_month <- as.integer(df2$publication_month)
names(df2)[names(df2) == "similar_book_id"] <- "book_id"
names(df2)[names(df2) == "reviews"] <- "text_reviews_count"
names(df2)[names(df2) == "rating"] <- "ratings_count"
names(df2)[names(df2) == "average.rating"] <- "average_rating"


#merge datasets using data.table
write.csv(df1, 'Data/df_giveaway.csv', row.names = F)
write.csv(df2, 'Data/df_similar.csv', row.names = F)

df1 <- fread("Data/df_giveaway.csv")
df2 <- fread("Data/df_similar.csv")

merged.df <- merge(df1, df2, 
                    all = T)
merged.df[is.na(merged.df)] <- 0

#remove if year-month is NA or 0
merged.df <- subset(merged.df, year_month != "NA-NA" & publication_year < 2023 & publication_year > 1000 & publication_month != 0)

#change giveaway to 0 and 1
merged.df[is.na(merged.df$giveaway)] <- 0

dfSummary(merged.df)

#save file
write.csv(booklist, 'Data/booklist.csv', row.names = F)
write.csv(merged.df, 'Data/merged_df.csv', row.names = F)




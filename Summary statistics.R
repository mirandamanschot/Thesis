#Summary statistics

setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries
library(dplyr)
library(summarytools)
library(sjPlot)
library(ggplot2)
library(psych)
library(data.table)
library(tidyverse)

booklist <- read.csv("Data/merged_df.csv")
raw <- fread("Data/reviews_thesis_notext.csv")
ratings <- fread("Data/ratings.csv")
df3 <- fread("Data/volume_df.csv")

dfSummary(df3)
#around 51% of dataset participates in giveaway
#around 25% only is an ebook, 70% is physical copy
sink('raw data.xlxs')
describe.by(raw)
sink()

#descriptive statistics

sink('descriptives.csv')
describeBy(ratings~ giveaway + booktype, skew = F)
sink()   

#graph reviews
jpeg("descriptive reviews.jpeg")
ggplot(ratings, aes(x = giveaway, y = text_reviews_count, fill = giveaway)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= 'mean', geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average review count per book type",
    x = "Giveaway",
    y = "Mean review count"
  )
dev.off()

#graph ratings
jpeg("descriptive ratings.jpeg")
ggplot(ratings, aes(x = giveaway, y = ratings, fill = giveaway)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= mean, geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average rating per book type",
    x = "Giveaway",
    y = "Mean rating"
  )
dev.off()

df4 <- ratings %>% group_by(book_id, booktype,giveaway) %>% 
  summarise(volume=n(),.groups = 'drop') %>% 
  as.data.frame()
View(df3)

#graph rating count
jpeg("descriptive rating count.jpeg")
ggplot(df4, aes(x = giveaway, y = volume, fill = giveaway)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= 'mean', geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average rating count per book type",
    x = "Giveaway",
    y = "Mean rating count"
  )
dev.off()

#graph winning oddst
jpeg("descriptive winning odds.jpeg")
ggplot(ratings, aes(x = giveaway, y = winning_odds, fill = giveaway)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= 'mean', geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average winning odds per book type",
    x = "Giveaway",
    y = "Mean winning odds"
  )
dev.off()

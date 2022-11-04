#Summary statistics

setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries
library(dplyr)
library(summarytools)
library(sjPlot)
library(ggplot2)
library(psych)
library(tidyverse)

booklist <- read.csv("Data/merged_df.csv")

dfSummary(booklist)
#around 51% of dataset participates in giveaway
#around 25% only is an ebook, 70% is physical copy

#change giveaway such that NaN = 0
booklist$giveaway[is.na(booklist$giveaway)] <- 0

#descriptive statistics

sink('descriptives.doc')
describeBy(booklist~ giveaway + booktype, skew = F)
sink()   

#graph reviews
jpeg("descriptive reviews.jpeg")
ggplot(booklist, aes(x = giveaway, y = text_reviews_count,)) + geom_bar(position = "dodge", stat = 'summary') +
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
ggplot(booklist, aes(x = giveaway, y = average_rating,)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= 'mean', geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average rating per book type",
    x = "Giveaway",
    y = "Mean rating"
  )
dev.off()
#graph rating count
jpeg("descriptive rating count.jpeg")
ggplot(booklist, aes(x = giveaway, y = ratings_count,)) + geom_bar(position = "dodge", stat = 'summary') +
  facet_wrap( ~ booktype, ncol = 3) +
  stat_summary(aes(label=round(..y..,2)), fun= 'mean', geom="text", size=3, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "Average rating count per book type",
    x = "Giveaway",
    y = "Mean rating count"
  )

dev.off()

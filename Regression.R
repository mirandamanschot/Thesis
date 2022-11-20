#Summary statistics

setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries
library(lfe)
library(plm)
library(data.table)
library(readr)
library(sjPlot)
library(ggplot2)
library(psych)
library(tidyverse)

booklist <- fread("Data/merged_df.csv")
booklist <- subset(booklist, average_rating != 0)
booklist$book_id <- as.factor(booklist$book_id)
regression_rating <- felm(average_rating ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds |  year_month , data = booklist)
summary(regression_rating)

regression_review <- felm(text_reviews_count ~ giveaway*factor(booktype) + giveaway*winning_odds | book_id + publication_year, data = booklist)
summary(regression_review)

regression_ratingcount <- lm(ratings_count ~ giveaway*factor(booktype) + giveaway*winning_odds | book_id + publication_year, data = booklist)
summary(regression_ratingcount)

#### Assumptions check ###


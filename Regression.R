#Summary statistics

setwd("C:/Users/miran/Documents/thesis_giveaways")

#libraries
install.packages("GGally")
library(lfe)
library(data.table)
library(readr)
library(sjPlot)
library(ggplot2)
library(psych)
library(tidyverse)
library(GGally)
library(car)
library(stargazer)

booklist <- read.csv("Data/merged_df.csv")
regression_rating <- felm(average_rating ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds|
                           year_month,
                          data = booklist)
summary(regression_rating)
intercept = getfe(regression_rating, ef='zm2')
summary(intercept)
vif(regression_rating)

regression_ratingcount <- felm(ratings_count ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds |  year_month, data = booklist)
summary(regression_ratingcount)
vif(regression_ratingcount)

regression_review <- felm(text_reviews_count ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds |  year_month, data = booklist)
summary(regression_review)
vif(regression_review)

sink('results.xlxs')
stargazer(regression_rating, regression_ratingcount, regression_review, title="Results", align = T, type = 'text', no.space=T, single.row = T)
sink()
#### Assumptions check ###
targazer(regression_rating, type = 'text', no.space=T, single.row = T)

summary(booklist)

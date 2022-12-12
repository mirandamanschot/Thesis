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

#New regressions
ratings <- fread("Data/ratings.csv")
df3 <- fread("Data/volume_df.csv")
regression_rating <- felm(ratings ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds|
                            book_id +
                            time,
                          data = ratings)
summary(regression_rating)
vif(regression_rating)


regression_volume <- felm(volume ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds|
                            timeym +
                            book_id,
                          data = df3)


sink('results_final.xlxs')
stargazer(regression_rating, regression_volume, title="Results", align = T, type = 'text', no.space=T, single.row = T)
sink()

dfSummary(ratings$booktype)

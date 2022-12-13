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
library(car)
library(stargazer)



ratings <- fread("Data/ratings.csv")
df3 <- fread("Data/volume_df.csv")

#New regressions
regression_rating <- felm(ratings ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds|
                            book_id +
                            time,
                          data = ratings)
summary(regression_rating)
vif(regression_rating)


regression_volume <- felm(log(volume) ~ giveaway + giveaway:factor(booktype) + giveaway:winning_odds|
                            timeym +
                            book_id,
                          data = df3)
summary(regression_volume)

sink('results_final.xlxs')
stargazer(regression_rating, regression_volume, title="Results", align = T, type = 'text', no.space=T, single.row = T)
sink()


#plot of residuals
res_rating <- resid(regression_rating)
res_volume <- resid(regression_volume)

jpeg("qqplot rating.jpeg")
qqnorm(res_rating, col = "steelblue", lwd = 2,
       main = "Normal Q-Q Plot Average Rating")
dev.off()

jpeg("qqplot volume.jpeg")
qqnorm(res_volume,  col = "steelblue", lwd = 2,
       main = "Normal Q-Q Plot Volume")
dev.off()


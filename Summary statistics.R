#Summary statistics

setwd("C:/Users/miran/Documents/thesis_giveaways")
install.packages('sjPlot')
#libraries
library(dplyr)
library(summarytools)
library(sjPlot)
library(ggplot2)
library(psych)
booklist <- read.csv("Data/booklist.csv")

dfSummary(booklist)
#around 10% of dataset participates in giveaway
#around 6% only is an ebook, 94% is physical copy

#descriptive statistics
descriptives <- booklist[c(1,19,23,24,33,34)] 


describeBy(descriptives ~ giveaway + booktype_dummy,
           skew=F)

tab_df(describeBy(descriptives~ giveaway + booktype_dummy, skew = F),
       title = "Descriptive statistics",
       file = "Descriptive_statistics.")

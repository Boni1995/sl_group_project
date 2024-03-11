# Library
library(dplyr)

# Load datasets

matches <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_1930_2022.csv")
immigration <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\MIG_05032024164943281.csv")

ds_matches <- matches[,c(1:3, 6)]

ds_matches$outcome <- ifelse(ds_matches$home_score > ds_matches$away_score, "w",
                             ifelse(ds_matches$home_score < ds_matches$away_score, "l", "t"))

colSums(is.na(matches))

sum(immigration$Variable == 'Acquisition of nationality by country of former nationality')

immigration_filter <-subset(immigration, Variable == "Acquisition of nationality by country of former nationality")

ds_immigration <- immigration_filter[,c(7:8, 11)]

colnames(ds_immigration) <- c("code", "country", "nationality_acquisition")


# Immigration dataset analysis

simple_avg <- aggregate(nationality_acquisition ~ country, data = ds_immigration, FUN = mean)

simple_avg$won <- sum

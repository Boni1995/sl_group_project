# Library
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)

# Load dataset
fifa_rnk = read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\fifa_ranking-2023-07-20.csv")

fifa_rnk <- fifa_rnk[,c("rank", "country_full", "total_points", "rank_date")]

colnames(fifa_rnk) <- c("rank", "country", "total_points", "date")

# Correct country names
country_corrections <- c("USA" = "United States", "Côte d'Ivoire" = "Ivory Coast", "Republic of Ireland", "Ireland",
                         "IR Iran" = "Iran", "China PR" = "China", "Turkey" = "Türkiye", "Northern Ireland" = "Ireland",
                         "Korea Republic" = "South Korea")

fifa_rnk$country <- ifelse(fifa_rnk$country %in% names(country_corrections),
                               country_corrections[fifa_rnk$country], fifa_rnk$country)

# Check countries in world cups
wc_countries = read.xlsx("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\countries.xlsx")

fifa_rnk$world_cups <- ifelse(fifa_rnk$country %in% wc_countries$country, 1, 0)

# Delete countries with no world cups played
fifa_rnk <- subset(fifa_rnk, !(world_cups == 0))

fifa_rnk <- fifa_rnk[,c("rank", "country", "total_points", "date")]

# Leave only years in the column date 
class(fifa_rnk$date)

fifa_rnk$year <- substr(fifa_rnk$date, 1, 4)
class(fifa_rnk$year)

fifa_rnk$year <- as.integer(fifa_rnk$year)
class(fifa_rnk$year)

# Final dataset
fifa_rnk <- fifa_rnk[,c("rank", "country", "total_points", "year")]

# Download the final dataset
path <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\ranking_fifa_final.csv"
write.csv(fifa_rnk, file = path)

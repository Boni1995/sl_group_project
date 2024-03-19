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

max(fifa_rnk$year)
min(fifa_rnk$year)

wc_years <- c(2022, 2022, 2022, 2022,
              2018, 2018, 2018, 2018,
              2014, 2014, 2014, 2014,
              2010, 2010, 2010, 2010,
              2006, 2006, 2006, 2006,
              2002, 2002, 2002, 2002,
              1998, 1998, 1998, 1998,
              1994, 1994, 1994, 1994)

full_years <- c(2022, 2021, 2020, 2019,
                2018, 2017, 2016, 2015,
                2014, 2013, 2012, 2011,
                2010, 2009, 2008, 2007,
                2006, 2005, 2004, 2003,
                2002, 2001, 2000, 1999,
                1998, 1997, 1996, 1995,
                1994, 1993, 1992, 1991)

year_map <- data.frame(year = full_years, reference_year = wc_years)

wc_fifa_rnk <- merge(fifa_rnk, year_map, by.x = "year", by.y = "year", all.x = TRUE)

summary(wc_fifa_rnk)

wc_fifa_rnk <- subset(wc_fifa_rnk, !(is.na(wc_fifa_rnk$reference_year)))

final_wc_rnk <- aggregate(. ~ reference_year + country, data = wc_fifa_rnk, FUN = mean)

final_wc_rnk <- final_wc_rnk[, c("reference_year", "country", "rank", "total_points")]
colnames(final_wc_rnk) <- c("year", "country", "rank", "total_points")

final_wc_rnk$rank <- round(final_wc_rnk$rank)
final_wc_rnk$total_points <- round(final_wc_rnk$total_points)


# Download the final dataset
path <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\ranking_fifa_final.csv"
write.csv(final_wc_rnk, file = path)

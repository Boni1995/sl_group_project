# Library
library(dplyr)
library(tidyr)
library(openxlsx)

# Load dataset
wc_matches = read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\wc_output.csv")
foreign = read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\foreign_born.csv")

path <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\foreign.xlsx"
write.xlsx(foreign, file = path)

# Correct country names
country_corrections <- c("Turkey" = "Türkiye", "Northern Ireland" = "Ireland", "Sweden " = "Sweden",
                         "East Germany" = "Germany", "West Germany" = "Germany", "Cameroon " = "Cameroon", "Republic of Ireland", "Ireland",
                         "SFR Yugoslavia" = "FR Yugoslavia", "Korea Republic" = "South Korea", "IR Iran" = "Iran",
                         "Côte d'Ivoire" = "Ivory Coast", "Korea DPR" = "North Korea", "China PR" = "China",
                         "Turkey" = "Türkiye")

# Replace corrected countries
wc_matches$home_team <- ifelse(wc_matches$home_team %in% names(country_corrections),
                           country_corrections[wc_matches$home_team], wc_matches$home_team)

wc_matches$away_team <- ifelse(wc_matches$away_team %in% names(country_corrections),
                               country_corrections[wc_matches$away_team], wc_matches$away_team)

foreign$International <- ifelse(foreign$International %in% names(country_corrections),
                               country_corrections[foreign$International], foreign$International)
colnames(foreign) <- c("country", "year", "foreign")

# Merge both datasets
matches_home_foreign <- merge(wc_matches, foreign, by.x = c("home_team", "Year"), by.y = c("country", "year"))

names(matches_home_foreign)[names(matches_home_foreign) == "foreign"] <- "home_foreign"


matches_foreign <- merge(matches_home_foreign, foreign, by.x = c("away_team", "Year"), by.y = c("country", "year"))

names(matches_foreign)[names(matches_foreign) == "foreign"] <- "away_foreign"

# Add FIFA ranking
fifa_rnk = read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\ranking_fifa_final.csv")

matches_foreign <- merge(matches_foreign, fifa_rnk, by.x = c("away_team", "Year"), by.y = c("country", "year"))
names(matches_foreign)[names(matches_foreign) == "rank"] <- "away_rank"
names(matches_foreign)[names(matches_foreign) == "total_points"] <- "away_points"

matches_foreign <- merge(matches_foreign, fifa_rnk, by.x = c("home_team", "Year"), by.y = c("country", "year"))

names(matches_foreign)[names(matches_foreign) == "rank"] <- "home_rank"
names(matches_foreign)[names(matches_foreign) == "total_points"] <- "home_points"

# Order and clean columns
matches_foreign <- matches_foreign[, c("Year", "home_team", "away_team", "home_score", "away_score", "outcome",
                                      "home_foreign", "home_rank", "home_points", "away_foreign", "away_rank",
                                      "away_points")]
str(matches_foreign)

# Download the final dataset
path <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_foreign_2.csv"
write.csv(matches_foreign, file = path)

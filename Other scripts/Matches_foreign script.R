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

names(matches_foreign)[names(matches_foreign) == "foreign"] <- "awaye_foreign"

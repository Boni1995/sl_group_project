# Library
library(dplyr)

# Load datasets

wc_matches <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_1930_2022.csv")
matches <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\results.csv")
ds_migration <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\MIG_05032024164943281.csv")

# Keep only teams' names and scores
ds_matches <- matches[,c(2:5)]

# Create a country index
all_countries <- c(ds_matches$home_team, ds_matches$away_team, ds_migration$Country)
unique_countries <- unique(all_countries)
countries_index <- data.frame(Country = unique_countries) # Copy this column and use it in excel to double check with migration countries

# Check availability of countries in each dataset
countries_index$home <- ifelse(countries_index$Country %in% ds_matches$home_team, 1, 0)
countries_index$away <- ifelse(countries_index$Country %in% ds_matches$away_team, 1, 0)
countries_index$migration <- ifelse(countries_index$Country %in% ds_migration$Country, 1, 0)

ruta_archivo <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\check_country_index.xlsx"
write.xlsx(countries_index, file = ruta_archivo)

country_corrections <- c("Czechia"="Czech Republic", "Czechoslovakia"="Czech Republic",
                         "Western Australia"="Australia","German DR"="Germany", "Northern Ireland"="Ireland",
                         "Republic of Ireland"="Ireland", "South Korea"="Korea", "Slovak Republic"="Slovakia",
                         "Central Spain"="Spain", "Turkey"="Türkiye", "United Kingdom"="England")

ds_matches$home_team <- ifelse(ds_matches$home_team %in% names(country_corrections),
                             country_corrections[ds_matches$home_team], ds_matches$home_team)

ds_matches$away_team <- ifelse(ds_matches$away_team %in% names(country_corrections),
                               country_corrections[ds_matches$away_team], ds_matches$away_team)

ds_migration$Country <- ifelse(ds_migration$Country %in% names(country_corrections),
                               country_corrections[ds_migration$Country], ds_migration$Country)

# Add a column to define if the home team won, lost or tie (for away team, we will later use an inverse count)
ds_matches$outcome <- ifelse(ds_matches$home_score > ds_matches$away_score, "w",
                             ifelse(ds_matches$home_score < ds_matches$away_score, "l", "t"))

# Create a dataset of immigration ("inflows of foreing population") and of
# ("Acquisition of nationality by country of former nationality") from the migration dataset

ds_migration_filter <- ds_migration[,c(4,8,11)]

immigration <- subset(ds_migration_filter, Variable == "Inflows of foreign population by nationality")

nationality <- subset(ds_migration_filter, Variable == "Acquisition of nationality by country of former nationality")

ds_immigration <- immigration[,c(2-3)]

ds_nationality <- nationality[,c(2-3)]


# Immigration dataset analysis

simple_avg <- aggregate(nationality_acquisition ~ country, data = ds_immigration, FUN = mean)

simple_avg$won <- sum

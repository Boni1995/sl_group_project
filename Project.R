# Library
library(dplyr)
library(tidyr)
library(openxlsx)

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

# ruta_archivo <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\check_country_index.xlsx"
# write.xlsx(countries_index, file = ruta_archivo)

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

# So final tables to use are: ds_matches, ds_immigration, ds_nationality

# We calculate avg immigration and nationality
immigration_avg <- aggregate(Value ~ Country, data = ds_immigration, FUN = mean)
colnames(immigration_avg) <- c("country", "annual_avg")

nationality_avg <- aggregate(Value ~ Country, data = ds_nationality, FUN = mean)
colnames(nationality_avg) <- c("country", "annual_avg")

# Now we will create a final table with all the variables in it, by country
# Matches results
ds_final_project <- ds_matches %>%
  pivot_longer(cols = c(home_team, away_team), names_to = "type", values_to = "country") %>%
  mutate(result = case_when(
    outcome == "w" & type == "home_team" | outcome == "l" & type == "away_team" ~ "w",
    outcome == "w" & type == "away_team" | outcome == "l" & type == "home_team" ~ "l",
    outcome == "t" ~ "t"
  )) %>%
  select(country, result)

# Add the immigration average
ds_final_project <- merge(ds_final_project, immigration_avg, by = "country", all.x = TRUE)
colnames(ds_final_project) <- c("country", "result", "immigration_avg")

ds_final_project <- merge(ds_final_project, nationality_avg, by = "country", all.x = TRUE)
colnames(ds_final_project) <- c("country", "result", "immigration_avg", "nationality_avg")

# Substract rows without data related to immigration
summary(ds_final_project)

ds_cleaned <- ds_final_project[!is.na(ds_final_project$nationality_avg),]

summary(ds_cleaned)

# World cup matches manipulation
wc_matches_2 <- wc_matches[,c(1:3,6)]
wc_matches_2$outcome <- ifelse(wc_matches_2$home_score > wc_matches_2$away_score, "w",
                               ifelse(wc_matches_2$home_score < wc_matches_2$away_score, "l", "t"))

wc_matches_3 <- wc_matches_2 %>%
  pivot_longer(cols = c(home_team, away_team), names_to = "type", values_to = "country") %>%
  mutate(result = case_when(
    outcome == "w" & type == "home_team" | outcome == "l" & type == "away_team" ~ "w",
    outcome == "w" & type == "away_team" | outcome == "l" & type == "home_team" ~ "l",
    outcome == "t" ~ "t"
  )) %>%
  select(country, result)
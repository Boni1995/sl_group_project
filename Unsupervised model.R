# Library
library(dplyr)
library(tidyr)
library(openxlsx)

# Load dataset
wc_players <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_foreign.csv")
wc_players

# Create separate tables for later join
home <- wc_players[ , c("home_team", "Year", "outcome", "home_foreign")]
visit <- home <- wc_players[, c("away_team", "Year", "outcome", "away_foreign")]

# Correct outcome for visit table
visit$outcome <- ifelse(visit$outcome == "w", "l",
                        ifelse(visit$outcome == "l", "w", "t"))

# Correct the column names

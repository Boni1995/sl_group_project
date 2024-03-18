matches = read.csv('wc_output.csv')
foreign_born = read.csv('foreign_born.csv')

colnames(matches)
colnames(foreign_born)


matches_foreign = merge(matches, foreign_born, 
                        by.x = c("home_team", "Year"), by.y = c("International", "FIFA.World.Cup"), all.x = TRUE)

matches_foreign = merge(matches_foreign, foreign_born, 
                        by.x = c("away_team", "Year"), by.y = c("International", "FIFA.World.Cup"), all.x = TRUE)


colnames(matches_foreign)

library(dplyr)
matches_foreign = matches_foreign %>%
  select(home_team, away_team, Year, everything())

names(matches_foreign) = c("home_team", "away_team", "Year", "home_score", "away_score", "outcome", "home_foreign", "away_foreign")

matches_foreign = na.omit(matches_foreign)

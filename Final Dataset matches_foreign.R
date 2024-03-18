matches = read.csv('wc_output.csv')
foreign_born = read.csv('foreign_born.csv')

# Correct country names
country_corrections <- c("Cameroon "="Cameroon", "China PR" = "China", "Czechoslavakia" = "Czechoslovakia",
                         "East Germany" = "Germany", "IR Iran" = "Iran", "Côte d'Ivoire" = "Ivory Coast",
                         "Korea DPR" = "North Korea", "Northern Ireland" = "Ireland", "Republic of Ireland" = "Ireland",
                         "SFR Yugoslavia" = "Yugoslavia", "Korea Republic" = "South Korea", "Sweden " = "Sweden",
                         "Turkey" = "Türkiye", "West Germany" = "Germany", "FR Yugoslavia" = "Yugoslavia",
                         "Germany DR" = "Germany")

matches$home_team <- ifelse(matches$home_team %in% names(country_corrections),
                            country_corrections[matches$home_team], matches$home_team)

matches$away_team <- ifelse(matches$away_team %in% names(country_corrections),
                            country_corrections[matches$away_team], matches$away_team)

foreign_born$International <- ifelse(foreign_born$International %in% names(country_corrections),
                                     country_corrections[foreign_born$International], foreign_born$International)

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

library(openxlsx)
path <- "C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_foreign.csv"
write.csv(matches_foreign, file = path)

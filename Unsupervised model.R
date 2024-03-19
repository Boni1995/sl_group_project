# Library
library(dplyr)
library(tidyr)
library(openxlsx)

foreign_born = read.xlsx("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\Dataset Two Approaches Migrants World Cup 1930-2018 HARVARD.xlsx")

# Number of rows same as foreign_born
foreign_born$foreign <- numeric(nrow(foreign_born))  

# Loop to add a column that assesses whether a football player is of foreign origins or not
for (i in 1:nrow(foreign_born)) {
  if (any(foreign_born[i, c("Nationality.Father", "Nationality.Mother", "Nationality.Grandfather", "Nationality.Grandmother", "Country.of.birth")] != foreign_born[i, "International"], na.rm = TRUE)) {
    foreign_born$foreign[i] <- 1
  } else {
    foreign_born$foreign[i] <- 0
  }
}

#Remove not needed columns
foreign_born <- foreign_born[, c("Name.Football.Player", "International", "FIFA.World.Cup", "foreign")]

colnames(foreign_born) <- c("name", "country", "year", "foreign")

# Correct country names
country_corrections <- c("Cameroon "="Cameroon", "China PR" = "China", "Czechoslavakia" = "Czechoslovakia",
                         "East Germany" = "Germany", "IR Iran" = "Iran", "Côte d'Ivoire" = "Ivory Coast",
                         "Korea DPR" = "North Korea", "Northern Ireland" = "Ireland", "Republic of Ireland" = "Ireland",
                         "SFR Yugoslavia" = "Yugoslavia", "Korea Republic" = "South Korea", "Sweden " = "Sweden",
                         "Turkey" = "Türkiye", "West Germany" = "Germany", "FR Yugoslavia" = "Yugoslavia",
                         "Germany DR" = "Germany")

foreign_born$country <- ifelse(foreign_born$country %in% names(country_corrections),
       country_corrections[foreign_born$country], foreign_born$country)

# Concatenate player and country
foreign_born$concat <- paste(foreign_born$name, foreign_born$country)

# Create dataset without duplications
foreigners <- foreign_born[!duplicated(foreign_born$concat), ]

df_foreigners <- aggregate(foreign ~ country, data = foreigners, FUN = sum) %>%
  select(country, foreign)

# Load dataset
wc_matches <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_foreign.csv")

wc_matches$home_team <- ifelse(wc_matches$home_team %in% names(country_corrections),
                               country_corrections[wc_matches$home_team], wc_matches$home_team)
wc_matches$away_team <- ifelse(wc_matches$away_team %in% names(country_corrections),
                               country_corrections[wc_matches$away_team], wc_matches$away_team)

# Create separate tables for later join
home <- wc_matches[ , c("home_team", "Year", "outcome", "home_foreign")]
colnames(home) <- c("country", "year", "outcome", "foreigners")

visit <- wc_matches[, c("away_team", "Year", "outcome", "away_foreign")]
colnames(visit) <- c("country", "year", "outcome", "foreigners")

# Correct outcome for visit table
visit$outcome <- ifelse(visit$outcome == "w", "l",
                        ifelse(visit$outcome == "l", "w", "t"))

# Join tables for final dataframe
df_wcups <- rbind(home, visit)


df_wcups$outcome <- ifelse(df_wcups$outcome == "w", 3,
                           ifelse(df_wcups$outcome == "l", 0, 1))

df_wcups2 <- aggregate(outcome ~ country, data = df_wcups, FUN = sum)

# Join both dataframes
df_country <- merge(df_wcups2, df_foreigners, by = "country", all.x = TRUE)

# Analyze dataset
library(ggplot2)

# Distribution of outcomes
hist(df_country$outcome,
     main = "Outcome Distribution",
     xlab = "Outcome",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# Top and bottom
top_bottom <- df_country[df_country$country %in% c("Canada", "China", "Germany", "Brazil", "Italy", "Argentina", "France"), ]
top_bottom <- top_bottom[order(top_bottom$outcome, decreasing = TRUE), ]

barplot(top_bottom$outcome,
        names.arg = top_bottom$country,
        main = "Top/Bottom countries",
        ylab = "Outcome",
        col = "skyblue",
        border = "black",
        ylim = c(0, max(top_bottom$outcome) * 1.2),
        las = 2)

# Distribution of foreigners
hist(df_country$foreign,
     main = "Number of foreigners playing World Cups",
     xlab = "n° of foreigners",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")

# Top and bottom
top_bottom_foreigners <- df_country[df_country$country %in% c("Canada", "China", "United States", "Germany", "Brazil", "Italy", "Argentina", "France"), ]
top_bottom_foreigners <- top_bottom_foreigners[order(top_bottom_foreigners$foreign, decreasing = TRUE), ]
usa <- c("United States" = "USA")
top_bottom_foreigners$country <- ifelse(top_bottom_foreigners$country %in% names(usa),
       usa[top_bottom_foreigners$country], top_bottom_foreigners$country)

barplot(top_bottom_foreigners$foreign,
        names.arg = top_bottom_foreigners$country,
        main = "Number of foreigners playing World Cups",
        ylab = "Foreigners",
        col = "skyblue",
        border = "black",
        ylim = c(0, max(top_bottom_foreigners$foreign) * 1.2),
        las = 2)


# Check normality
# shapiro.test(df_by_country$foreigners)
# shapiro.test(df_by_country$score)
# log_results <- log(results_by_country$result)
# shapiro.test(log_results)
# ks.test(results_by_country$result, "pnorm", mean(results_by_country$result), sd(results_by_country$result))

# Analyzing to reduce data for dendogram
summary(df_country)

# Defining number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df_elbow <- df_country[,c(2:3)]

wssplot(df_elbow, nc=15)

# Distances
distances <- dist(df_country[,c("outcome", "foreign")], method = "euclidean")

# Dendogram
H.fit <- hclust(distances, method="ward.D2")

plot(H.fit, labels = df_country$country, main = "Cluster by score and foreigners", cex = 0.8) # display dendogram

# draw dendogram with red borders around the clusters
rect.hclust(H.fit, k=4, border="red") 


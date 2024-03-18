# Library
library(dplyr)
library(tidyr)
library(openxlsx)

# Load dataset
wc_players <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\matches_foreign.csv")
wc_players

# Create separate tables for later join
home <- wc_players[ , c("home_team", "Year", "outcome", "home_foreign")]
colnames(home) <- c("country", "year", "outcome", "foreigners")

visit <- wc_players[, c("away_team", "Year", "outcome", "away_foreign")]
colnames(visit) <- c("country", "year", "outcome", "foreigners")

# Correct outcome for visit table
visit$outcome <- ifelse(visit$outcome == "w", "l",
                        ifelse(visit$outcome == "l", "w", "t"))

# Join tables for final dataframe
df_wcups <- rbind(home, visit)


df_wcups$outcome <- ifelse(df_wcups$outcome == "w", 3,
                           ifelse(df_wcups$outcome == "l", 0, 1))

# Summarize by country by year
df_by_country <- df_wcups %>%
  group_by(country, year, foreigners) %>%
  summarize(score = sum(outcome))

# Check normality
# shapiro.test(df_by_country$foreigners)
# shapiro.test(df_by_country$score)
# log_results <- log(results_by_country$result)
# shapiro.test(log_results)
# ks.test(results_by_country$result, "pnorm", mean(results_by_country$result), sd(results_by_country$result))

# Threshold for score
mean(df_by_country$score)
summary(df_by_country)

df_dendogram <- df_by_country[df_by_country$score > 6,]


# Defining number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df_elbow <- df_by_country[,c(3:4)]

wssplot(df_elbow, nc=15)

# Distances
distances <- dist(df_by_country[,c("foreigners", "score")], method = "euclidean")

# Dendogram
H.fit <- hclust(distances, method="ward.D2")

plot(H.fit, labels = df_by_country$country, main = "Cluster by score and foreigners",
     cex.main=1) # display dendogram

#groups <- cutree(H.fit, k=3) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 
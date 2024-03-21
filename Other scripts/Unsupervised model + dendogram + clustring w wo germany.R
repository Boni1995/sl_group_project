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
hist(df_country$outcome, freq = FALSE, main = "Histograma de Datos", xlab = "Valores", ylab = "Densidad")
hist(df_country$foreign, freq = FALSE, main = "Histograma de Datos", xlab = "Valores", ylab = "Densidad")


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


head(df_country)

# Calculate the Pearson correlation coefficient for the entire dataframe
correlation_coef <- cor(df_country$outcome, df_country$foreign, use="complete.obs")

# Print the correlation coefficient
print(correlation_coef)
# Visualize the relationship with a scatter plot for the entire dataframe
plot(df_country$foreign, df_country$outcome,
     main="Outcome vs. Number of Foreigners",
     xlab="Number of Foreigners", ylab="Outcome (Score)",
     pch=19, col="blue")


# Standardize the data for clustering
df_for_clustering_scaled <- scale(df_country[, c("outcome", "foreign")])

# Elbow method to determine the optimal number of clusters
wss <- numeric(15)
for (i in 1:15) {
  set.seed(123) # Ensure reproducibility
  km_out <- kmeans(df_for_clustering_scaled, centers=i, nstart=25)
  wss[i] <- km_out$tot.withinss
}

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Perform k-means clustering with the chosen number of clusters, e.g., k=3
set.seed(123)
k <- 3 # Replace with your chosen k based on the elbow plot
km_result <- kmeans(df_for_clustering_scaled, centers=k, nstart=25)

# Add cluster assignments to your dataframe
df_country$cluster <- as.factor(km_result$cluster)

# View the distribution of countries across the clusters
table(df_country$cluster)
aggregate(cbind(outcome, foreign) ~ cluster, data=df_country, FUN=mean)
# Assuming df_country now includes a 'cluster' column from the k-means result

# Plotting the clusters
library(ggplot2)

# Plotting the clusters with country labels
ggplot(df_country, aes(x=foreign, y=outcome, label=country)) +
  geom_point(alpha=0.7, size=3) +  # Plot the points
  geom_text(aes(color=as.factor(cluster)), check_overlap = TRUE, vjust = -0.5) +  # Add text labels
  scale_color_manual(values=rainbow(length(unique(df_country$cluster)))) +
  labs(title="K-means Clustering of Countries by Outcome and Number of Foreigners",
       x="Number of Foreigners", y="Outcome (Score)",
       color="Cluster") +
  theme_minimal() +
  theme(legend.position="right")










# Filter out Germany from the dataframe
df_country_filtered <- df_country %>% filter(country != "Germany")

# Standardize the data for clustering without Germany
df_for_clustering_scaled_filtered <- scale(df_country_filtered[, c("outcome", "foreign")])

# Determine the optimal number of clusters again (optional, you can use the same k you found previously)
wss_filtered <- numeric(15)
for (i in 1:15) {
  set.seed(123) # Ensure reproducibility
  km_out_filtered <- kmeans(df_for_clustering_scaled_filtered, centers=i, nstart=25)
  wss_filtered[i] <- km_out_filtered$tot.withinss
}

plot(1:15, wss_filtered, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Assuming the same k value is chosen
set.seed(123)
k <- 3 # or the new k value based on the elbow plot without Germany
km_result_filtered <- kmeans(df_for_clustering_scaled_filtered, centers=k, nstart=25)

# Add cluster assignments to the filtered dataframe
df_country_filtered$cluster <- as.factor(km_result_filtered$cluster)

# View the distribution of countries across the clusters without Germany
table(df_country_filtered$cluster)

# Plotting the clusters without Germany
library(ggplot2)

ggplot(df_country_filtered, aes(x=foreign, y=outcome, label=country)) +
  geom_point(aes(color=as.factor(cluster)), alpha=0.7, size=3) +
  geom_text(check_overlap = TRUE, vjust = -0.5) +
  scale_color_manual(values=rainbow(length(unique(df_country_filtered$cluster)))) +
  labs(title="K-means Clustering of Countries by Outcome and Number of Foreigners (without Germany)",
       x="Number of Foreigners", y="Outcome (Score)",
       color="Cluster") +
  theme_minimal() +
  theme(legend.position="right")



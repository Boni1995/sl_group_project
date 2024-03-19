# Library
library(dplyr)
library(tidyr)
library(openxlsx)

# Country corrections
country_corrections <- c("Czechia"="Czech Republic",
                         "Western Australia"="Australia","German DR"="Germany", "Northern Ireland"="Ireland",
                         "Republic of Ireland"="Ireland", "South Korea"="Korea", "Slovak Republic"="Slovakia",
                         "Central Spain"="Spain", "Turkey"="Türkiye", "United Kingdom"="England")

# Load datasets

matches <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\results.csv")
ds_migration <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_group_project\\Datasets\\MIG_05032024164943281.csv")

# Obtain wanted columns
ds_matches <- matches[,c(2:5)]

# Add outcome
ds_matches$outcome <- ifelse(ds_matches$home_score > ds_matches$away_score, "w",
                             ifelse(ds_matches$home_score < ds_matches$away_score, "l", "t"))

# Add value
ds_matches2 <- ds_matches %>%
  pivot_longer(cols = c(home_team, away_team), names_to = "type", values_to = "country") %>%
  mutate(result = case_when(
    outcome == "w" & type == "home_team" | outcome == "l" & type == "away_team" ~ "w",
    outcome == "w" & type == "away_team" | outcome == "l" & type == "home_team" ~ "l",
    outcome == "t" ~ "t"
  )) %>%
  select(country, result)

ds_matches2$country <- ifelse(ds_matches2$country %in% names(country_corrections),
                             country_corrections[ds_matches2$country], ds_matches2$country)


# Final table with summation of matches
result_by_country <- ds_matches2 %>%
  group_by(country, result) %>%
  summarise(total = n()) %>%
  pivot_wider(names_from = result, values_from = total, values_fill = 0)

country_counts <- table(ds_matches2$country)

result_by_country$wins_n <- result_by_country$w/country_counts[result_by_country$country]
result_by_country$loss_n <- result_by_country$l/country_counts[result_by_country$country]
result_by_country$ties_n <- result_by_country$t/country_counts[result_by_country$country]

ds_results <- result_by_country[,c(1, 5:7)]

# Add nationality acquisition
ds_migration_filter <- ds_migration[,c(4,8,11)]

nationality <- subset(ds_migration_filter, Variable == "Acquisition of nationality by country of former nationality")
colnames(nationality) <- c("type", "country", "people")

nationality$country <- ifelse(nationality$country %in% names(country_corrections),
                             country_corrections[nationality$country], nationality$country)

nationality_avg <- aggregate(people ~ country, data = nationality, FUN = mean)
colnames(nationality_avg) <- c("country", "annual_avg")

ds_final <- merge(ds_results, nationality_avg, by = "country", all.x = TRUE)
colnames(ds_final) <- c("country", "wins_n", "loss_n", "ties_n", "nationality_avg")

df_cleaned <- ds_final[!is.na(ds_final$nationality_avg),]

summary(df_cleaned)

# Check normality
# shapiro.test(results_by_country$result)
# log_results <- log(results_by_country$result)
# shapiro.test(log_results)
# ks.test(results_by_country$result, "pnorm", mean(results_by_country$result), sd(results_by_country$result))

# Standarize values`
results_for_stand <- result_by_country[,c(1:4)]
data_for_standarize <- merge(results_for_stand, nationality_avg, by = "country", all.x = TRUE)
colnames(data_for_standarize) <- c("country", "wins_n", "loss_n", "ties_n", "nationality_avg")

final_data_for_stand <- data_for_standarize[!is.na(data_for_standarize$nationality_avg),]

data_to_standariz <- final_data_for_stand[,c("wins_n", "loss_n", "ties_n", "nationality_avg")]
scaled_data <- scale(data_to_standariz)

min_value <- min(df_cleaned$nationality_avg)
max_value <- max(df_cleaned$nationality_avg)
df_cleaned_stand <- df_cleaned
df_cleaned_stand$nationality_avg <- (df_cleaned_stand$nationality_avg - min_value)/(max_value - min_value)

data_to_standariz2 <- df_cleaned_stand[,c("wins_n", "loss_n", "ties_n", "nationality_avg")]

# Defining number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df_elbow <- df_cleaned[,c(2:5)]

wssplot(df_elbow, nc=15)
wssplot(scaled_data, nc=15)
wssplot(data_to_standariz2, nc=15)

# Distances
distances <- dist(df_cleaned[,c("wins_n", "loss_n", "ties_n", "nationality_avg")], method = "euclidean")
stand_distances <- dist(scaled_data, method = "euclidean")
stan2_distances <- dist(data_to_standariz2, method = "euclidean")


# Original Dendogram
H.fit <- hclust(distances, method="ward.D2")

plot(H.fit, labels = df_cleaned$country, main = "Cluster by matches' outcomes and nationality acquisition",
     cex.main=1) # display dendogram

#groups <- cutree(H.fit, k=3) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 


# Scaled Dendogram (4 columns standarized together)
H.fit <- hclust(stand_distances, method="ward.D2")

plot(H.fit, labels = final_data_for_stand$country, main = "Cluster by matches' outcomes and nationality acquisition",
     cex.main=1) # display dendogram

#groups <- cutree(H.fit, k=3) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=4, border="red") 


# Scaled Dendogram (nationality column)
H.fit <- hclust(stan2_distances, method="ward.D2")

plot(H.fit, labels = df_cleaned_stand$country, main = "Cluster by matches' outcomes and nationality acquisition",
     cex.main=1) # display dendogram

#groups <- cutree(H.fit, k=3) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red")


# Check levels of nationality acquisition (maybe a histogram)

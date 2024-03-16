# Library
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("openxlsx")
library(openxlsx)

# Load datasets

wc_matches <- read.csv("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\matches_1930_2022.csv")
matches <- read.csv("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\results.csv")
ds_migration <- read.csv("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\MIG_05032024164943281.csv")

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


# Write the cleaned data to an Excel file
write.xlsx(ds_cleaned, file = ("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\Final_Cleaned_Data.xlsx")

           
           
           
install.packages("readxl")
library(readxl)
# Assuming your Final_Cleaned_Data.xlsx is in the current working directory
ds_cleaned <- read_excel("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\Final_Cleaned_Data.xlsx")


library(Rtsne)

# Assuming 'features' contains your 'immigration_avg' and 'nationality_avg' columns
features <- ds_cleaned[, c("immigration_avg", "nationality_avg")]

# Add a very small amount of noise to the data to make each row unique
set.seed(42) # Ensure reproducibility
noise <- matrix(runif(n = nrow(features) * ncol(features), min = -1e-5, max = 1e-5), nrow = nrow(features), ncol = ncol(features))
features_noisy <- features + noise

# Run t-SNE on the slightly perturbed dataset
tsne_results <- Rtsne(features_noisy, dims = 2, perplexity = 30)

# Plotting
plot(tsne_results$Y[,1], tsne_results$Y[,2], main = "t-SNE", col = as.factor(ds_cleaned$result), pch = 20, xlab = "", ylab = "")
legend("topright", legend = levels(as.factor(ds_cleaned$result)), col = 1:length(levels(as.factor(ds_cleaned$result))), pch = 20)



install.packages("umap")
library(umap)
install.packages("ggplot2")
library(ggplot2)

# Assuming 'features' contains your 'immigration_avg' and 'nationality_avg' columns
# We're using the 'ds_cleaned' data which you've read from 'Final_Cleaned_Data.xlsx'
features <- ds_cleaned[, c("immigration_avg", "nationality_avg")]

# Apply UMAP
set.seed(42) # For reproducibility
umap_results <- umap(features)

# Convert the UMAP output to a data frame for plotting
umap_df <- as.data.frame(umap_results$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$result <- ds_cleaned$result

# Plot the UMAP results using ggplot2
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = result)) +
  geom_point(alpha = 0.6) +
  labs(title = "UMAP projection of the dataset", x = "UMAP Dimension 1", y = "UMAP Dimension 2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) # You can change the colors if you want





# Assuming you have installed and loaded the necessary libraries and your dataset is loaded as 'ds_cleaned'
library(ggplot2)

# Selecting the features for clustering
features <- ds_cleaned[, c("immigration_avg", "nationality_avg")]

# Standardizing the features may lead to better results
features <- scale(features)

# Determining the optimal number of clusters using the elbow method
wcss <- sapply(1:10, function(k){kmeans(features, k, nstart = 10)$tot.withinss})
plot(1:10, wcss, type = "b", xlab = "Number of clusters (K)", ylab = "Total within-clusters sum of squares")

# Assuming we choose K after analyzing the plot (let's say K = 3 for example)
set.seed(42) # For reproducibility
kmeans_result <- kmeans(features, centers = 3, nstart = 10)

# Adding the cluster assignment to your dataset
ds_cleaned$cluster <- kmeans_result$cluster

# Plotting the clusters
ggplot(ds_cleaned, aes(x = immigration_avg, y = nationality_avg, color = as.factor(cluster))) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "K-Means Clustering of the Dataset", x = "Immigration Average", y = "Nationality Average") +
  theme_minimal()



library(ggplot2)

# Assuming 'ds_cleaned' now includes a 'cluster' column from K-Means and a 'result' column with match outcomes
ggplot(ds_cleaned, aes(x = immigration_avg, y = nationality_avg)) +
  geom_point(aes(color = result), position = position_jitter(width = 0.2, height = 0.2), size = 3, alpha = 0.6) + 
  scale_color_manual(values = c('green' = 'win', 'red' = 'loss', 'blue' = 'tie')) +
  facet_wrap(~cluster) + # Creates a separate plot for each cluster
  labs(title = "K-Means Clustering with Match Outcomes", x = "Immigration Average", y = "Nationality Average") +
  theme_minimal()

ds_training <- read.csv("C:\\Users\\diana\\OneDrive\\Desktop\\Datasets\\ds_training.csv")

# Load the dataset
# Calculate the score
ds_training$scores <- with(ds_training, losses * 0 + ties * 1 + wins * 3)

# If you want to calculate the total score for each country
country_scores <- aggregate(scores ~ country, data = ds_training, sum)

# Print out the first few rows of the new dataframe
head(country_scores)
head(ds_training)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

ds_training$scores <- normalize(ds_training$scores)
ds_training$nationality_acquisition_avg <- normalize(ds_training$nationality_acquisition_avg)


features_for_clustering <- ds_training[, c('scores', 'nationality_acquisition_avg')]

# Determine the optimal number of clusters, let's say we've chosen 3 for this example
set.seed(42) # for reproducibility
kmeans_result <- kmeans(features_for_clustering, centers = 3, nstart = 20)

# Add the cluster assignments to your dataset
ds_training$cluster <- kmeans_result$cluster

# Visualize the clusters
library(ggplot2)


library(ggplot2)

# Assuming ds_training now includes 'scores', 'immigration_avg', and 'cluster' columns
# Make sure ds_training$country contains the country names

# Plotting with country names
ggplot(ds_training, aes(x = scores, y = nationality_acquisition_avg, label = country)) +
  geom_text(aes(color = as.factor(cluster)), check_overlap = TRUE, size = 3) + # Display country names with colors by cluster
  labs(title = "K-Means Clustering on Football Data", x = "Scores", y = "Immigration Average") +
  theme_minimal() +
  scale_color_manual(values = c("yellow", "pink", "black")) # Specify your colors for clusters


# Assuming ds_training has columns 'scores' and 'immigration_avg' already scaled
correlation <- cor(ds_training$scores, ds_training$immigration_inflow_avg, method = "pearson")

# Print the correlation
print(correlation)


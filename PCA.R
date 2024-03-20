library(dplyr)
pca_data <- read.csv("C:\\Users\\Hossein\\Downloads\\sl_group_project-main (8)\\sl_group_project-main\\Datasets\\matches_foreign_final.csv")

# Select columns after the 4th column
pca_data <- pca_data[, 5:ncol(pca_data)]

pca_data <- pca_data %>%
  mutate(
    win = ifelse(outcome == "w", 1, 0),
    loss = ifelse(outcome == "l", 1, 0),
    tie = ifelse(outcome == "t", 1, 0)
  ) %>%
  select(-outcome) # Remove the original outcome column

pca_data <- pca_data %>%
  select(home_score, home_foreign, home_rank, home_points, away_score, away_foreign, away_rank, away_points, win, loss, tie)

scaled_data <- scale(pca_data[, 1:8])

# Convert the scaled matrix back to a dataframe
scaled_data <- as.data.frame(scaled_data)

# Add column names back to the dataframe
data <- cbind(scaled_data, pca_data[,9:11])
data <- data %>%
  rename(home_win = win, away_win = loss)
data

# Perform PCA
pca_result <- prcomp(data, scale. = TRUE)

# Analyze the results
summary(pca_result)


pc_scores <- pca_result$x

# Plot the first two principal components
plot(pc_scores[,1], pc_scores[,2], 
     xlab = "Principal Component 1", 
     ylab = "Principal Component 2",
     main = "PCA")

# Add labels for each point (optional)
text(pc_scores[,1], pc_scores[,2], labels = row.names(pc_scores))

# Add a legend for the outcome (if available)
if("outcome_numeric" %in% colnames(data)) {
  legend("topright", legend = levels(data$outcome_numeric), pch = 1, col = 1:length(levels(data$outcome_numeric)))
}


# Increase the size of the plotting area
par(mar = c(5, 5, 4, 2) + 0.1)

# Plot the loadings of the first two principal components without dots
plot(pca_result$rotation[,1], pca_result$rotation[,2], 
     type = "n", # 'n' means no plotting
     xlab = "PC1 Loading", 
     ylab = "PC2 Loading",
     main = "Loadings Plot",
     xlim = range(pca_result$rotation[,1]) + c(-0.1, 0.1), # Adjust the x-axis limits
     ylim = range(pca_result$rotation[,2]) + c(-0.1, 0.1)) # Adjust the y-axis limits

# Add labels for variables
text(pca_result$rotation[,1], pca_result$rotation[,2], labels = rownames(pca_result$rotation), pos = 3, col = "blue")

# Calculate the center of the plot
center_x <- mean(range(pca_result$rotation[,1]))
center_y <- mean(range(pca_result$rotation[,2]))

# Draw red arrows from the center to each loading
arrows(center_x, center_y, pca_result$rotation[,1], pca_result$rotation[,2], 
       col = "green", length = 0.1)

# Plot the loadings of the first two principal components
biplot(pca_result, scale = 0, arrows = TRUE)

# Add dots for variables instead of labels
points(pca_result$rotation[,1], pca_result$rotation[,2], col = "blue", pch = 19)

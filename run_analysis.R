# Example usage of receiver_epa_analysis.R

# First, make sure you have the required packages
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("nflfastR", quietly = TRUE)) {
  install.packages("nflfastR")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load the analysis functions
source("receiver_epa_analysis.R")

# Load NFL play-by-play data using nflfastR
# You can choose any season from 1999-2023
pbp_data <- nflfastR::load_pbp(2023)  # For 2023 season

# Print the number of observations
cat("Loaded", nrow(pbp_data), "plays from the 2023 season\n")

# Run the receiver EPA analysis
cat("Running receiver EPA analysis...\n")
results <- run_receiver_analysis(pbp_data)

# View the results
# 1. EPA per target statistics by position
cat("\n=== EPA per Target Statistics by Position ===\n")
print(head(results$receiver_stats, 10))  # Show first 10 rows

# 2. Display the visualization
cat("\n=== Displaying EPA Visualization ===\n")
print(results$epa_plot)

# 3. Team insights and recommendations
cat("\n=== Team Insights and Recommendations ===\n")
print(head(results$team_insights, 10))  # Show first 10 rows

# Team-specific analysis example
cat("\n=== Team-specific Analysis (Kansas City Chiefs) ===\n")
chiefs_data <- pbp_data %>%
  filter(posteam == "KC")

cat("Analyzing", nrow(chiefs_data), "plays for the Kansas City Chiefs\n")
chiefs_results <- run_receiver_analysis(chiefs_data)
print(chiefs_results$receiver_stats)
print(chiefs_results$epa_plot)

# Optional: Save results to CSV
cat("\n=== Saving Results to CSV ===\n")
write.csv(results$receiver_stats, "receiver_stats.csv", row.names = FALSE)
write.csv(results$team_insights, "team_insights.csv", row.names = FALSE)
cat("Results saved to receiver_stats.csv and team_insights.csv\n")

cat("\nAnalysis complete!\n") 
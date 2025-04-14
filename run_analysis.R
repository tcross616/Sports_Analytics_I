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
tryCatch({
  write.csv(results$receiver_stats, "receiver_stats.csv", row.names = FALSE)
  write.csv(results$team_insights, "team_insights.csv", row.names = FALSE)
  cat("Results saved to receiver_stats.csv and team_insights.csv\n")
}, error = function(e) {
  cat("Error saving results to CSV:", e$message, "\n")
})

# Save team-specific results
tryCatch({
  write.csv(chiefs_results$receiver_stats, "chiefs_receiver_stats.csv", row.names = FALSE)
  cat("Chiefs results saved to chiefs_receiver_stats.csv\n")
}, error = function(e) {
  cat("Error saving Chiefs results to CSV:", e$message, "\n")
})

# Generate a plot highlighting the KC position groups compared to league average
cat("\n=== Creating Comparison Visualization ===\n")
tryCatch({
  # Calculate league average EPA per target by position
  league_avg <- results$receiver_stats %>%
    group_by(receiver_position) %>%
    summarize(
      league_avg_epa = mean(epa_per_target, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create comparison data
  if (nrow(chiefs_results$receiver_stats) > 0) {
    comparison_data <- chiefs_results$receiver_stats %>%
      left_join(league_avg, by = "receiver_position") %>%
      mutate(
        diff_from_avg = epa_per_target - league_avg_epa,
        better_than_avg = diff_from_avg > 0
      )
    
    # Create comparison plot
    comparison_plot <- ggplot(comparison_data, aes(x = receiver_position, y = diff_from_avg, fill = better_than_avg)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(
        title = "Chiefs EPA/Target vs. League Average by Position",
        x = "Position",
        y = "Difference from League Average EPA/Target",
        fill = "Better than Average"
      ) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
      theme_minimal()
    
    print(comparison_plot)
    
    # Save plot
    ggsave("chiefs_vs_league_avg.png", comparison_plot, width = 8, height = 6)
    cat("Comparison visualization saved to chiefs_vs_league_avg.png\n")
  } else {
    cat("Insufficient Chiefs data for comparison visualization\n")
  }
}, error = function(e) {
  cat("Error creating comparison visualization:", e$message, "\n")
})

cat("\nAnalysis complete!\n") 
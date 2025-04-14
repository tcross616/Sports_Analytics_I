# Load required packages
library(tidyverse)
library(nflfastR)
library(ggplot2)

# Function to calculate EPA per target by position
analyze_receiver_epa <- function(plays_data) {
  # Filter for pass plays and calculate EPA per target
  receiver_stats <- plays_data %>%
    filter(play_type == "pass") %>%
    group_by(receiver_position, posteam) %>%
    summarise(
      total_targets = n(),
      total_epa = sum(epa, na.rm = TRUE),
      epa_per_target = total_epa / total_targets,
      .groups = "drop"
    ) %>%
    filter(!is.na(receiver_position)) %>%
    filter(receiver_position %in% c("WR", "RB", "TE"))
  
  return(receiver_stats)
}

# Function to create visualization
plot_receiver_epa <- function(receiver_stats) {
  ggplot(receiver_stats, aes(x = receiver_position, y = epa_per_target, fill = receiver_position)) +
    geom_boxplot() +
    labs(
      title = "EPA per Target by Receiver Position",
      x = "Position",
      y = "EPA per Target",
      fill = "Position"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Function to identify team-specific insights
get_team_insights <- function(receiver_stats) {
  team_insights <- receiver_stats %>%
    group_by(posteam) %>%
    summarise(
      best_position = receiver_position[which.max(epa_per_target)],
      best_epa = max(epa_per_target),
      worst_position = receiver_position[which.min(epa_per_target)],
      worst_epa = min(epa_per_target),
      .groups = "drop"
    ) %>%
    mutate(
      recommendation = case_when(
        best_position == "TE" ~ "Consider increasing TE usage in passing game",
        best_position == "RB" ~ "Consider increasing RB usage in passing game",
        best_position == "WR" ~ "Current WR usage is optimal",
        TRUE ~ "No clear recommendation"
      )
    )
  
  return(team_insights)
}

# Main analysis function
run_receiver_analysis <- function(plays_data) {
  # Calculate receiver stats
  receiver_stats <- analyze_receiver_epa(plays_data)
  
  # Create visualization
  epa_plot <- plot_receiver_epa(receiver_stats)
  
  # Get team insights
  team_insights <- get_team_insights(receiver_stats)
  
  # Return results
  list(
    receiver_stats = receiver_stats,
    epa_plot = epa_plot,
    team_insights = team_insights
  )
}

# Example usage:
# results <- run_receiver_analysis(plays_data)
# print(results$epa_plot)
# View(results$team_insights) 
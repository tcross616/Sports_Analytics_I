# Load required packages
library(tidyverse)
library(nflfastR)
library(ggplot2)

# Function to calculate EPA per target by position
analyze_receiver_epa <- function(plays_data) {
  # Find the correct receiver column name
  receiver_cols <- c("receiver_player_position", "receiver_position", "receiver")
  receiver_col <- receiver_cols[receiver_cols %in% names(plays_data)][1]
  
  if (is.na(receiver_col)) {
    stop("No receiver position column found in the data")
  }
  
  # Filter for pass plays and calculate EPA per target
  receiver_stats <- plays_data %>%
    filter(play_type == "pass") %>%
    # Use the correct column name dynamically
    group_by(!!sym(receiver_col), posteam) %>%
    reframe(
      total_targets = n(),
      total_epa = sum(epa, na.rm = TRUE),
      epa_per_target = ifelse(total_targets > 0, total_epa / total_targets, 0),
      .groups = "drop"
    ) %>%
    filter(!is.na(!!sym(receiver_col))) %>%
    # Update position filters to match nflfastR position codes
    filter(!!sym(receiver_col) %in% c("WR", "RB", "TE")) %>%
    # Rename column for consistency
    rename(receiver_position = !!sym(receiver_col))
  
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
  # First handle teams with data
  team_insights <- receiver_stats %>%
    group_by(posteam) %>%
    # Check if we have at least one valid value for each position
    filter(n() >= 3) %>% # Need data for at least 3 positions to compare
    summarize(
      has_data = all(!is.na(epa_per_target) & is.finite(epa_per_target)),
      .groups = "drop"
    ) %>%
    filter(has_data)
  
  # If no teams have complete data, return empty dataframe with structure
  if(nrow(team_insights) == 0) {
    return(data.frame(
      posteam = character(),
      best_position = character(),
      best_epa = numeric(),
      worst_position = character(),
      worst_epa = numeric(),
      recommendation = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Process only teams with valid data
  team_insights <- receiver_stats %>%
    filter(posteam %in% team_insights$posteam) %>%
    group_by(posteam) %>%
    summarize(
      positions_present = list(unique(receiver_position)),
      position_count = n_distinct(receiver_position),
      best_position = if(any(!is.na(epa_per_target))) {
        receiver_position[which.max(epa_per_target)]
      } else {
        NA_character_
      },
      best_epa = if(any(!is.na(epa_per_target))) {
        max(epa_per_target, na.rm = TRUE)
      } else {
        NA_real_
      },
      worst_position = if(any(!is.na(epa_per_target))) {
        receiver_position[which.min(epa_per_target)]
      } else {
        NA_character_
      },
      worst_epa = if(any(!is.na(epa_per_target))) {
        min(epa_per_target, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(
      recommendation = case_when(
        !is.na(best_position) & best_position == "TE" ~ "Consider increasing TE usage in passing game",
        !is.na(best_position) & best_position == "RB" ~ "Consider increasing RB usage in passing game",
        !is.na(best_position) & best_position == "WR" ~ "Current WR usage is optimal",
        TRUE ~ "Insufficient data for recommendation"
      )
    )
  
  return(team_insights)
}

# Main analysis function
run_receiver_analysis <- function(plays_data) {
  # Calculate receiver stats
  receiver_stats <- analyze_receiver_epa(plays_data)
  
  # Create visualization only if we have data
  if(nrow(receiver_stats) > 0) {
    epa_plot <- plot_receiver_epa(receiver_stats)
  } else {
    epa_plot <- NULL
    warning("No receiver data found. Cannot create visualization.")
  }
  
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
# 1. First, load the NFL play-by-play data
# pbp_data <- nflfastR::load_pbp(2023)  # For 2023 season
# 
# 2. Run the analysis
# results <- run_receiver_analysis(pbp_data)
# 
# 3. View the results
# print(results$receiver_stats)
# print(results$epa_plot)
# print(results$team_insights)
# 
# 4. Save to CSV (optional)
# write.csv(results$receiver_stats, "receiver_stats.csv", row.names = FALSE)
# write.csv(results$team_insights, "team_insights.csv", row.names = FALSE) 
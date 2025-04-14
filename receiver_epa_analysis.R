# Load required packages
library(tidyverse)
library(nflfastR)
library(ggplot2)

# Function to load player position data
load_player_positions <- function() {
  # Get player position data from nflfastR
  # This includes all NFL players with their positions
  players <- nflfastR::fast_scraper_roster(seasons = 2023)
  
  # Return the position data
  return(players)
}

# Function to calculate EPA per target by position
analyze_receiver_epa <- function(plays_data) {
  # Load player position data
  player_positions <- load_player_positions()
  
  # Filter for pass plays
  pass_plays <- plays_data %>%
    filter(play_type == "pass") %>%
    filter(!is.na(receiver_player_id)) # Only include plays with a receiver
  
  # Get receiver position by joining with player data
  receiver_stats <- pass_plays %>%
    left_join(
      player_positions %>% 
        select(player_id = gsis_id, position),
      by = c("receiver_player_id" = "player_id")
    ) %>%
    # Filter for WR, RB, TE positions
    filter(position %in% c("WR", "RB", "TE")) %>%
    # Group by position and team
    group_by(position, posteam) %>%
    # Calculate EPA stats
    summarise(
      total_targets = n(),
      total_epa = sum(epa, na.rm = TRUE),
      epa_per_target = ifelse(total_targets > 0, total_epa / total_targets, 0),
      .groups = "drop"
    ) %>%
    # Rename for consistency with original code
    rename(receiver_position = position)
  
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
    # Check if we have data for all positions (WR, RB, TE)
    summarize(
      position_count = n_distinct(receiver_position),
      has_data = position_count >= 3,
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
      # Instead of storing positions as a list, store as comma-separated string
      positions_present = paste(unique(receiver_position), collapse = ","),
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

library(nflfastR)
library(dplyr)
library(mclust)

# Load nflfastR play-by-play data for the 2021 season
data <- load_pbp(2021)

# Coin-flip function for various events (fumble, interception, incompletion)
coin_flip <- function(probability) {
  return(runif(1) < probability)
}

# Example probabilities for coin-flips (these are rough estimates)
fumble_prob <- pbp_data %>%
  summarise(prob = mean(fumble == 1, na.rm = TRUE))
interception_prob <- pbp_data %>%
  filter(play_type == "pass") %>%
  summarise(prob = mean(interception == 1, na.rm = TRUE)) %>%
  pull(prob)
incompletion_prob <-pbp_data %>%
  filter(play_type == "pass") %>%
  summarise(prob = mean(!complete_pass, na.rm = TRUE)) 

# Function to simulate events: fumble, interception, incompletion
simulate_play_events <- function(play_data) {
  fumble <- coin_flip(fumble_prob)
  interception <- coin_flip(interception_prob)
  incompletion <- coin_flip(incompletion_prob)
  
  return(c(fumble = fumble, interception = interception, incompletion = incompletion))
}

# Function to classify plays into red zone or non-red zone
define_field_zone <- function(yardline_100) {
  # Red zone is within 20 yards of the opponent's end zone
  if (yardline_100 <= 20) {
    return("Red Zone")
  } else {
    return("Non-Red Zone")
  }
}

# Function to adjust yards based on field zone
handle_end_zone <- function(yards_gained, field_zone) {
  # If the play happens in the non-red zone and yards gained exceeds the field length
  if (field_zone == "Non-Red Zone" && yards_gained > 100) {
    return(7)  # Touchdown
  } else if (field_zone == "Red Zone" && yards_gained > 40) {
    return(7)  # Touchdown
  } else {
    return(yards_gained)  # Normal yards gained
  }
}

# Simulate yards gained based on play type (run or pass)
simulate_yards <- function(play_data, model_parameters) {
  # Determine play type (pass or run)
  play_type <- play_data$play_type
  
  # If the play is an incompletion, return 0 yards
  if (play_data$incompletion) {
    return(0)
  }
  
  # Sample yards based on the play type
  if (play_type == "pass") {
    # Sample yards for a passing play (just as an example)
    pass_yds <- nflfastR::load_pbp(2021) %>%
      filter(play_type == "pass") %>%
      summarise(mean_yards = mean(yards_gained, na.rm = TRUE))
    
    return(pass_yds$mean_yards)
  } else {
    # Sample yards for a running play
    run_yds <- nflfastR::load_pbp(2021) %>%
      filter(play_type == "run") %>%
      summarise(mean_yards = mean(yards_gained, na.rm = TRUE))
    
    return(run_yds$mean_yards)
  }
}

# Function to simulate a football play
simulate_play <- function(play_data, model_parameters) {
  # Extract data for the specific play
  play_type <- play_data$play_type
  yardline_100 <- play_data$yardline_100
  
  # Simulate events (fumble, interception, incompletion)
  events <- simulate_play_events(play_data)
  
  # Classify field zone (red zone or non-red zone)
  field_zone <- define_field_zone(yardline_100)
  
  # Simulate yards gained
  yards_gained <- simulate_yards(play_data, model_parameters)
  
  # Adjust yards based on the field zone
  final_yards <- handle_end_zone(yards_gained, field_zone)
  
  return(list(
    "play_type" = play_type,
    "field_zone" = field_zone,
    "yards_gained" = final_yards,
    "events" = events
  ))
}

# Example usage
# Sampled a random play from the dataset (you can select based on any conditions)
random_play <- data[sample(1:nrow(data), 1), ]

# Define model parameters for the Gaussian Mixture Model (we can adjust as needed)
model_parameters <- list(
  num_components = 3,
  means = c(2, 15, 30),  # Example: short, medium, long gains
  covariances = c(1, 2, 3),
  weights = c(0.5, 0.3, 0.2)
)

# Simulating the play and getting the results
play_results <- simulate_play(random_play, model_parameters)

# Output the results
print(paste("Play Type: ", play_results$play_type))
print(paste("Field Zone: ", play_results$field_zone))
print(paste("Final Yards Gained: ", play_results$yards_gained))
print(paste("Fumble: ", play_results$events["fumble"]))
print(paste("Interception: ", play_results$events["interception"]))
print(paste("Incompletion: ", play_results$events["incompletion"]))
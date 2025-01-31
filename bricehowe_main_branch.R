game_data <- readRDS("data.rds")
head(game_data)

# Load necessary libraries
library(dplyr)

# Read the dataset
game_data <- readRDS("data.rds")

# Get a list of unique teams
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
num_teams <- length(teams)

# Create an empty transition matrix
transition_matrix <- matrix(0, nrow = num_teams, ncol = num_teams, dimnames = list(teams, teams))


# Fill in the matrix based on game outcomes
for (i in 1:nrow(game_data)) {
  home_team <- game_data$Home_Team[i]
  away_team <- game_data$Visiting_Team[i]
  
  home_score <- game_data$Home_Score[i]
  away_score <- game_data$Visiting_Score[i]
  
  if (home_score > away_score) {
    transition_matrix[away_team, home_team] <- transition_matrix[away_team, home_team] + 1
  } else if (away_score > home_score) {
    transition_matrix[home_team, away_team] <- transition_matrix[home_team, away_team] + 1
  }
}

# Normalize each row to ensure it's a valid probability matrix
transition_matrix <- transition_matrix / rowSums(transition_matrix, na.rm = TRUE)


set.seed(42)  # For reproducibility

# Number of iterations
total_iterations <- 20000
burn_in <- 1000  # First 1000 transitions are ignored

# Initialize coin with a random team
current_team <- sample(teams, 1)

# Store counts of how often each team holds the coin
coin_counts <- setNames(rep(0, num_teams), teams)

# Perform random walk
for (i in 1:total_iterations) {
  # Get the transition probabilities for the current team
  probabilities <- transition_matrix[current_team, ]
  
  # Choose the next team based on transition probabilities
  next_team <- sample(teams, 1, prob = probabilities, replace = TRUE)
  
  # Update team coin counts (ignore burn-in period)
  if (i > burn_in) {
    coin_counts[next_team] <- coin_counts[next_team] + 1
  }
  
  # Move to next team
  current_team <- next_team
}

# Convert counts to probabilities (percentage of time each team held the coin)
rankings <- data.frame(Team = names(coin_counts), Score = coin_counts / sum(coin_counts))

# Sort rankings in descending order
rankings <- rankings %>% arrange(desc(Score))

# Print final rankings
print(rankings)




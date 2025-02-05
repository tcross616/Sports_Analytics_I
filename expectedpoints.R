

expected_points <- function(down, ytg, fp) {
  # Initialize a numeric vector to store scores from multiple simulations
  n<-1000
  scores <- rep(NA, n)
  
  # Run n simulations (default 1000) to estimate expected points
  for (i in 1:n) {
    scores[i] <- run_epoch(down, ytg, fp)  # Simulate one game scenario and store the score
  }
  
  # Return the average score over all simulations, representing expected points
  mean(scores)
}

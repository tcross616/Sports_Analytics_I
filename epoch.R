run_epoch <- function(down, ytg, fp) {
  score <- 0  # Initialize score to 0 (no score at the beginning of the epoch)
  team <- 1   # 1 represents the "reference team," -1 represents the "opponent"
  drives <- 0 # Initialize drive count to track the number of drives in the epoch

  # Loop until a score occurs OR a maximum of 10 drives is reached
  while (score == 0 && drives < 10) {
    state <- run_drive(down, ytg, fp)  # Simulate a drive and return the updated game state
    score <- process_drive_result(state)  # Check if a score occurred from the updated state

    # If a score occurs, adjust it based on which team scored (+ for reference, - for opponent)
    if (score != 0) score <- team * score 

    team <- -team  # Switch possession: If the reference team had the ball, give it to the opponent, and vice versa
    drives <- drives + 1  # Increment drive count to prevent infinite loops
  }
  
  score  # The last evaluated expression is implicitly returned
}

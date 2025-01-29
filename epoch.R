run_epoch <- function(down, ytg, fp) {
  score <- 0
  team <- 1 # 1 for "reference team", -1 for "opponent"
  drives <- 0 
  

  while (score == 0 && drives < 10) {
    state <- run_drive(down, ytg, fp) 
    # Simulate a drive and return the updated game state
    score <- process_drive_result(state)
     # Check if a score occurred from the updated state
     
      # # If a score occurs, adjust it based on which team scored 
     #(+ for reference, - for opponent)
    if (score != 0) score <- team * score 
    team <- -team # Switch possession: If the reference team had the ball, 
    # give it to the opposite and vice versa
    drives <- drives + 1 # Increment drive count to prevent infinite loops
  }
  
  score # The last evaluated expression is implicitly returned
}

down_four <- function(ytg, fp) {
  # If the yards to go (ytg) is less than 2 and the field position (fp) is beyond the 50-yard line,
  # the team is more likely to go for it on fourth down.
  if (ytg < 2 && fp > 50) {  
    outcome <- "go_for_it"
  } 
  # Otherwise, if the field position is greater than 63, the team decides between punting or attempting a field goal.
  # There is a 70% chance of punting and a 30% chance of attempting a field goal.
  else if (fp > 63) {  
    outcome <- sample(c("punt", "field_goal"), 1, prob = c(0.3, 0.7))
  } 
  # If the field position is 63 or less, the team always punts.
  else {  
    outcome <- "punt"
  }

  # Handling the case where the team decides to go for it.
  if (outcome == "go_for_it") {
    # Simulating the yards gained or lost on the play.
    # The probabilities favor small gains, with a small chance of losing yards.
    yards_gained <- sample(-2:5, 1, prob = c(rep(0.2, 2), rep(0.6, 5)))
    
    # Updating the new field position after the play.
    new_fp <- fp + yards_gained
    
    # Updating the new yards to go for the first down.
    new_ytg <- max(ytg - yards_gained, 1)  # Ensures ytg never goes below 1
    
    # If the team gains enough yards for a first down, reset the down to 1.
    # Otherwise, they are still on fourth down.
    if (new_ytg <= 0) {
      # First down achieved, drive continues
      list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
    } else {
      # Turnover on downs, opponent takes over with flipped field position
      list(down = 1, ytg = 10, fp = 100 - new_fp, exit_drive = 1)
    }
  } 
  # Handling the case where the team attempts a field goal.
  else if (outcome == "field_goal") {
    # Simulating whether the field goal attempt is successful.
    # The probability of success is 65%.
    field_goal_success <- sample(c(TRUE, FALSE), 1, prob = c(0.65, 0.35))
    
    if (field_goal_success) {
      # If successful, the team scores and the opponent starts from their own 25-yard line
      list(down = 1, ytg = 10, fp = 25, exit_drive = 1)
    } else {
      # If missed, the opponent takes over possession at the previous fp minus 7 yards,
      # with the field position flipped.
      list(down = 1, ytg = 10, fp = 100 - (fp - 7), exit_drive = 1)
    }
  } 
  # Handling the punt scenario.
  else {  
    # Simulating the punt distance (between 35 and 50 yards).
    punt_distance <- sample(35:50, 1)
    
    # The opponent takes over possession after the punt, ensuring fp does not go below 0.
    # The field position is flipped so that it is correct for the new possession.
    list(down = 1, ytg = 10, fp = 100 - max(fp - punt_distance, 0), exit_drive = 1)
  }
}

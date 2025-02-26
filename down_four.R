down_four <- function(ytg, fp) {
  
  #    Using our multinomial model to decide between three options.
  #    Since nflfastR dataset typically defines 'yardline_100' as the yards
  #    away from the defense's end zone, and our 'fp' is yards from the
  #    offense's own end zone, we changed yardline_100 = 100 - fp.
  
  yardline_100 <- 100 - fp
  prob_decisions <- estimate_decision_probabilities(yardline = yardline_100, ytg = ytg)

  outcome <- sample(
    x    = c("go_for_it", "punt", "field_goal"),
    size = 1,
    prob = prob_decisions
  )
  # Executing whichever outcome was chosen
  
  if (outcome == "go_for_it") {
    # Simulating yards gained
    yards_gained <- sample(-2:5, 1, prob = c(0.2, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
    
    new_fp <- fp + yards_gained
    new_ytg <- max(ytg - yards_gained, 1)  
    
    if (new_ytg <= 0) {
      # First down achieved: new set of downs, continue the drive
      list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
    } else {
      # Failed conversion => turnover on downs => flip field position
      list(down = 1, ytg = 10, fp = 100 - new_fp, exit_drive = 1)
    }
    
  } else if (outcome == "field_goal") {
    # using logistic regression model for FG success
    # (don't forget) yardline_100 is the distance from the opponent's end zone
    prob_success <- estimate_fg_success(yardline_100)
    field_goal_success <- sample(c(TRUE, FALSE), 1,
                                 prob = c(prob_success, 1 - prob_success))
    
    if (field_goal_success) {
      # If successful, we assume a made FG => exit_drive = 1 (they scored)
      # Opponent starts at their 25-yard line, for example
      list(down = 1, ytg = 10, fp = 25, exit_drive = 1)
    } else {
      # Missed FG => Opponent takes over around (fp - 7) yards downfield (your choice)
      list(down = 1, ytg = 10, fp = 100 - (fp - 7), exit_drive = 1)
    }
    
  } else {
    # outcome == "punt"
    # Simulate a punt distance (uniform between 35 and 50 yards, for example)
    punt_distance <- sample(35:50, 1)
    new_fp <- fp - punt_distance
    # Flip field for opponent
    list(down = 1, ytg = 10, fp = 100 - max(new_fp, 0), exit_drive = 1)
  }
}

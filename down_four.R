down_four <- function(ytg, fp) {
  if (ytg < 2 && fp > 50) {  # More likely to go for it if close to the first down & in opponent territory
    outcome <- "go_for_it"
  } else if (fp > 30) {
    outcome <- sample(c("punt", "field_goal"), 1, prob = c(0.7, 0.3))
  } else {
    outcome <- "punt"
  }
  
  if (outcome == "go_for_it") {
    yards_gained <- sample(-2:5, 1, prob = c(rep(0.2, 2), rep(0.6, 5)))
    new_fp <- fp + yards_gained
    new_ytg <- max(ytg - yards_gained, 1)
    down <- ifelse(new_ytg <= 0, 1, 4)

    return(list(down = down, ytg = new_ytg, fp = new_fp, exit_drive = 0))
  } else if (outcome == "field_goal") {
    field_goal_success <- sample(c(TRUE, FALSE), 1, prob = c(0.8, 0.2))
    if (field_goal_success) {
      return(list(down = 1, ytg = 10, fp = 115, exit_drive = 1))
    } else {
      return(list(down = 1, ytg = 10, fp = fp - 7, exit_drive = 1))
    }
  } else {  # Punt scenario
    punt_distance <- sample(35:50, 1)
    return(list(down = 1, ytg = 10, fp = max(fp - punt_distance, 0), exit_drive = 1))
  }
}
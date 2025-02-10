down_one <- function(ytg, fp) {
  yards_gained <- sample(-5:20, 1, prob = c(rep(0.02, 5), rep(0.08, 20)))  # More likely positive gain
  new_fp <- fp + yards_gained
  new_ytg <- max(ytg - yards_gained, 1)
  down <- ifelse(new_ytg <= 0, 1, 2)  # Reset to 1st down if achieved

  list(down = down, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}
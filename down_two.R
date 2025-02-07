down_two <- function(ytg, fp) {
  yards_gained <- sample(-5:15, 1, prob = c(rep(0.02, 5), rep(0.09, 15)))
  new_fp <- fp + yards_gained
  new_ytg <- max(ytg - yards_gained, 1)
  down <- ifelse(new_ytg <= 0, 1, 3)

  return(list(down = down, ytg = new_ytg, fp = new_fp, exit_drive = 0))
}

run_drive <- function(down, ytg, fp) {
  possible_outcomes <- c(-10, -5, 0, 5, 10, 20) 
  yardage_change <- sample(possible_outcomes, 1)
  
  new_fp <- fp + yardage_change
  
  if (yardage_change >= ytg) {
    down <- 1
    ytg <- 10
  } else {
    down <- down + 1
    ytg <- max(0, ytg - yardage_change)
  }
  
  list(
    down = down,
    ytg = ytg,
    fp = new_fp
  )
}

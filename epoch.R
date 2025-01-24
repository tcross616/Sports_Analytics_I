run_epoch <- function(down, ytg, fp) {
  score <- 0
  team <- 1
  drives <- 0
  

  while (score == 0 && drives < 10) {
    state <- run_drive(down, ytg, fp)
    score <- process_drive_result(state)
    if (score != 0) score <- team * score 
    team <- -team
    drives <- drives + 1
  }
  
  return(score)
}

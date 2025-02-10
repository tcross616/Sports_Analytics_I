source("run_play.R")

# run_drive() repeatedly calls run_play() until the play signals that the drive is over.
run_drive <- function(down, ytg, fp) {
  new_state <- run_play(down, ytg, fp)
  
  if(new_state$exit_drive == 0) {
    # Continue the drive with the new state.
    run_drive(new_state$down, new_state$ytg, new_state$fp)
  } else {
    # Drive is finished, so return the state for the epoch-level processing.
    list(down = new_state$down, ytg = new_state$ytg, fp = new_state$fp)
  }
}
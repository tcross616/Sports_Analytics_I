run_play <- function(down, ytg, fp) {
  if (down == 1) {
    return(down_one(ytg, fp))
  } else if (down == 2) {
    return(down_two(ytg, fp))
  } else if (down == 3) {
    return(down_three(ytg, fp))
  } else {
    return(down_four(ytg, fp))
  }
}
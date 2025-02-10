run_play <- function(down, ytg, fp) {
  if (down == 1) {
    down_one(ytg, fp)
  } else if (down == 2) {
    down_two(ytg, fp)
  } else if (down == 3) {
    down_three(ytg, fp)
  } else {
    down_four(ytg, fp)
  }
}
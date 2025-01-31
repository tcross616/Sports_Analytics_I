process_drive_result <- function(fp) {
  if (fp > 80) return(7)
  if (fp > 40) return(3) 
  if(fp < 20) return(-3)
  if (fp < 0) return(-7)   
  return(0)                
}
process_drive_result(90)
expected_points <- simulate_epoch(down = 1, ytg = 10, fp =70)
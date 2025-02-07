source("run_epoch.R")

# This is our top level function. This is the function we will call when we want
# to compute an expected points for a certain "state" of the game.  For now,
# the states are: down, ytg, and fp.

get_EP <- function(down, ytg, fp) {
  # create a vector to store points from our simulation.  For now, we will run
  # 1000.  This is something that a user might want to control, so later we 
  # will allow it to be passed in as an argument.
  
  n <- 1000 # Always try to avoid "hard coding" by putting numbers that _could_
  # change into the "working part" of the code, especially if that
  # number will need to appear more than once, which it will here.
  # Otherwise, you have to make sure you change it everywhere in your
  # code if you want to change it.  By defining it here, we need only
  # change it once.
  
  # create a vector to store our simulations
  points <- rep(NA, n)
  
  # for now we can just use a for loop to actually run the simulations.  There
  # are more efficient ways to do this, but we aren't expecting simulations to 
  # take _that_ long.
  
  for(i in 1:n) {
    points[i] <- run_epoch(down, ytg, fp)
  }
  
  # And now we return the mean!  You should avoid "return" statements.  They 
  # require a bunch of overhead and are very inefficient.  Instead, just don't
  # save the last thing you do to a variable.
  
  mean(points)
  
}

################################################################################
############################# Test that get_EP works ###########################
################################################################################

# We don't really need to test this at this point.  Remember we are really
# working on the run_epoch code in the main branch.  If you want to test the
# get_EP function, you should really go to the "working_get_EP_function" branch.
get_EP(2, 5, 34)
get_EP(1, 10, 61)
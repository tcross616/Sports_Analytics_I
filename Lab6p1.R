shots_df <- read.csv("nhl_pbp20162017.csv")
library(dplyr)
library(ggplot2)
shot
# Filter all shot-related events
shots_df <- shots_df %>%
  #filtering into shots for all possible outcomes of a shot
  filter(Event %in% c("SHOT", "GOAL", "MISS", "BLOCK")) %>%
  mutate(
    # Making Period numeric and seconds as numeric
    Seconds_Elapsed = as.numeric(Seconds_Elapsed),
    Period = as.numeric(Period), 
    point_diff = Home_Score - Away_Score,  # Point Differential to determine shot rate
    shot_on_goal = ifelse(Event %in% c("GOAL", "BLOCK"), 1, 0), # define shot on goal as a goal or block
    time_block = cut(Seconds_Elapsed %% 1200, 
                     breaks = c(0, 400, 800, 1200), #separating time into the periods
                     labels = c("Early", "Mid", "Late"), #Type of 
                     include.lowest = TRUE)
  )

#Shot Rate Model using the point differential
shot_rate_model <- glm(shot_on_goal ~ time_block + point_diff + Ev_Team, 
                       data = shots_df, 
                       family = binomial())
#Summary of the model
summary(shot_rate_model)

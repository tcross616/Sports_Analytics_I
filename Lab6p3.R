# Specify a coach name to filter the data
selected_coach <- "GUY BOUCHER"
coach_shots_df <- shots_df %>% filter(Home_Coach == selected_coach | Away_Coach == selected_coach)
coach_shots_df

coach_shots_df <- shots_df %>%
  #filtering into shots for all possible outcomes of a shot for one specific coach
  filter(Event %in% c("SHOT", "GOAL", "MISS", "BLOCK")) %>%
  mutate(
    # Making Period numeric and seconds as numeric
    Seconds_Elapsed = as.numeric(Seconds_Elapsed),
    Period = as.numeric(Period), 
    point_diff = Home_Score - Away_Score,  # Point Differential to determine shot rate
    shot_on_goal = ifelse(Event %in% c("GOAL", "BLOCK"), 1, 0),
    time_block = cut(Seconds_Elapsed %% 1200, 
                     breaks = c(0, 400, 800, 1200), #seperating time into the periods
                     labels = c("Early", "Mid", "Late"), #Type of 
                     include.lowest = TRUE)
  )
shot_rate_coach <- glm(shot_on_goal ~ time_block + point_diff + Ev_Team, 
                       data = coach_shots_df, 
                       family = binomial())
summary(shot_rate_coach)
# Visualizing Shot Success Probability using Gaussian Distribution
ggplot(coach_shots_df, aes(x = xC, y = yC)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = paste("Shot Success Probability for", selected_coach))


# Specify a coach name to filter the data
selected_coach <- "GUY BOUCHER"
coach_shots_df <- shots_df %>% filter(Home_Coach == selected_coach | Away_Coach == selected_coach)
coach_shots_df
# Visualizing Shot Success Probability using Gaussian Distribution
ggplot(coach_shots_df, aes(x = xC, y = yC)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = paste("Shot Success Probability for", selected_coach))


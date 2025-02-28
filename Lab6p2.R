setwd("C:/Users/puckh/Desktop/Brice Documents/Fourth Year/Sports Analytics")
df <- read.csv("nhl_pbp20162017.csv")

library(ggplot2)
library(dplyr)
library(MASS)  
library(ggpubr)


# Filter Shot, goal, miss
df_shots <- df %>% filter(Event %in% c("SHOT", "GOAL","MISS"))
df_shots <- dplyr::select(df_shots, xC, yC)

### ---- OPTION 1: Define Ice Regions ---- ###


x_bins <- c(-100, -50, 0, 50, 100) 
y_bins <- c(-42.5, -20, 0, 20, 42.5)  


df_shots$x_region <- cut(df_shots$xC, breaks = x_bins, labels = c(1, 2, 3, 4))
df_shots$y_region <- cut(df_shots$yC, breaks = y_bins, labels = c(1, 2, 3, 4))
df_shots$zone <- paste(df_shots$x_region, df_shots$y_region, sep = "-")


shot_rate_per_region <- df_shots %>% 
  group_by(zone) %>% 
  summarise(shot_count = n())

print("Shot Rate Per Region:")
print(shot_rate_per_region)

# Plot shot density heatmap
ggplot(df_shots, aes(x = xC, y = yC)) +
  geom_bin2d(bins = 30, alpha = 0.6) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Shot Density Heatmap", x = "xC (Rink Location)", y = "yC (Rink Location)")


### ---- OPTION 2: Poisson Regression for Shot Rate Prediction ---- ###

# Creating shot count per unique location
shot_counts <- df_shots %>% 
  group_by(xC, yC) %>% 
  summarise(shot_count = n())

# Splitting the data into train and test sets
set.seed(42)
train_indices <- sample(1:nrow(shot_counts), 0.8 * nrow(shot_counts))
train_data <- shot_counts[train_indices, ]
test_data <- shot_counts[-train_indices, ]

# Train Poisson Regression Model
poisson_model <- glm(shot_count ~ xC + yC, family = poisson(link = "log"), data = train_data)

# Predict shot rates
test_data$predicted_shot_rate <- predict(poisson_model, newdata = test_data, type = "response")

# Compute Mean Squared Error (MSE)
mse <- mean((test_data$shot_count - test_data$predicted_shot_rate)^2)
print(paste("Mean Squared Error (MSE):", round(mse, 4)))

# Scatterplot of actual vs predicted shot rates
ggplot(test_data, aes(x = shot_count, y = predicted_shot_rate)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Actual vs Predicted Shot Rate",
       x = "Actual Shot Rate",
       y = "Predicted Shot Rate") +
  theme_minimal()






### ---- OPTION 2: Poisson Regression for Shot Rate Prediction ---- ###

# Creating shot count per unique location
shot_counts <- df_shots %>% 
  group_by(xC, yC) %>% 
  summarise(shot_count = n())

# Splitting the data into train and test sets
set.seed(42)
train_indices <- sample(1:nrow(shot_counts), 0.8 * nrow(shot_counts))
train_data <- shot_counts[train_indices, ]
test_data <- shot_counts[-train_indices, ]

# Train Poisson Regression Model
poisson_model <- glm(shot_count ~ xC + yC, family = poisson(link = "log"), data = train_data)

# Predict shot rates
test_data$predicted_shot_rate <- predict(poisson_model, newdata = test_data, type = "response")

# Compute Mean Squared Error (MSE)
mse <- mean((test_data$shot_count - test_data$predicted_shot_rate)^2)
print(paste("Mean Squared Error (MSE):", round(mse, 4)))

# Scatterplot of actual vs predicted shot rates
ggplot(test_data, aes(x = shot_count, y = predicted_shot_rate)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Poisson Regression: Actual vs Predicted Shot Rate",
       x = "Actual Shot Rate",
       y = "Predicted Shot Rate") +
  theme_minimal()
library(nflfastR)
library(dplyr)

# Step 1: Get the data
field_goal_data <- nflfastR::load_pbp(2020) %>%
  filter(play_type == "field_goal") %>%
  select(yardline_100, field_goal_result)

# Step 2: Prepare the data
field_goal_data <- field_goal_data %>%
  mutate(success = ifelse(field_goal_result == "made", 1, 0))

# Step 3: Fit the logistic regression model
model <- glm(success ~ yardline_100, data = field_goal_data, family = binomial)

# Step 4: Incorporate the model into your simulation
# Example function to estimate probability of success
estimate_fg_success <- function(yardline) {
  prob_success <- predict(model, newdata = data.frame(yardline_100 = yardline), type = "response")
  return(prob_success)
}

# Example usage
yardline <- 40  # Example field position
probability <- estimate_fg_success(yardline)
print(paste("Estimated probability of success for a field goal from", yardline, "yards:", probability))


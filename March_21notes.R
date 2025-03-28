# Install necessary packages if not already installed
install.packages(c("nflfastR", "dplyr", "mixtools","em
"))
seasons <- 2020
pbp_data <- load_pbp(seasons)
glimpse(pbp_data)
play_type_probs <- pbp_data %>%
  filter(play_type %in% c("run", "pass")) %>%
  count(play_type) %>%
  mutate(prob = n / sum(n))
incompletion_prob <- pbp_data %>%
  filter(play_type == "pass") %>%
  summarise(prob = mean(!complete_pass, na.rm = TRUE))
fumble_prob <- pbp_data %>%
  summarise(prob = mean(fumble == 1, na.rm = TRUE))


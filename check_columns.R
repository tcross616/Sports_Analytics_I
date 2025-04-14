# Script to check what columns are available in nflfastR data

# Load nflfastR
library(nflfastR)

# Load a sample of the data
pbp_data <- nflfastR::load_pbp(2023)  # Load 2023 season data

# Display basic information
cat("Number of plays:", nrow(pbp_data), "\n")
cat("Number of columns:", ncol(pbp_data), "\n\n")

# Check for all columns related to receivers or positions
cat("Checking for receiver-related columns:\n")
receiver_cols <- grep("receiver|position|player|pass_|target", names(pbp_data), value = TRUE)
print(receiver_cols)

# Check if there are specific columns for pass plays
cat("\nChecking for pass play indicators:\n")
pass_cols <- grep("pass|throw|qb|quarterback", names(pbp_data), value = TRUE)
print(pass_cols)

# Check for specific receiver position columns
cat("\nChecking for 'receiver_player_position' column specifically:\n")
if ("receiver_player_position" %in% names(pbp_data)) {
  cat("Column exists!\n")
  # Show values
  cat("Sample values:\n")
  print(head(table(pbp_data$receiver_player_position)))
} else {
  cat("Column does not exist\n")
}

# Check for any columns that might contain receiver position info
cat("\nLooking for other columns that might contain position info:\n")
position_cols <- grep("position", names(pbp_data), value = TRUE)
for (col in position_cols) {
  cat("\nColumn:", col, "\n")
  # Show top values if not all NA
  if (all(is.na(pbp_data[[col]]))) {
    cat("All values are NA\n")
  } else {
    cat("Sample values:\n")
    print(head(table(pbp_data[[col]]), 10))
  }
}

# Show a sample of a pass play
cat("\nSample of a pass play:\n")
sample_pass <- pbp_data[pbp_data$play_type == "pass", ][1, ]
print(names(sample_pass))

# Check for columns containing WR, RB, TE values
cat("\nChecking if any columns contain WR, RB, TE values:\n")
for (col in names(pbp_data)) {
  if (is.character(pbp_data[[col]]) || is.factor(pbp_data[[col]])) {
    values <- unique(pbp_data[[col]])
    if (any(c("WR", "RB", "TE") %in% values, na.rm = TRUE)) {
      cat("Column:", col, "\n")
      cat("Sample values:\n")
      print(head(table(pbp_data[[col]]), 10))
      cat("\n")
    }
  }
} 
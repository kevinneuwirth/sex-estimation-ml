### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Load required packages
library(dplyr)
library(summarytools)

# Choose and read the CSV file
loaddata <- choose.files(default = "Select database", caption = "Select database", multi = FALSE)
data <- read.csv(loaddata,check.names = FALSE)

# Function to calculate statistics for numeric columns
calc_stats <- function(x) {
  list(
    "Average" = mean(x, na.rm = TRUE),
    "Median" = median(x, na.rm = TRUE),
    "Minimum" = min(x, na.rm = TRUE),
    "Maximum" = max(x, na.rm = TRUE),
    "Std. Deviation" = sd(x, na.rm = TRUE),
    "Variance" = var(x, na.rm = TRUE),
    "Lower Quartile (25%)" = quantile(x, probs = 0.25, na.rm = TRUE),
    "Upper Quartile (75%)" = quantile(x, probs = 0.75, na.rm = TRUE),
    "Interquartile Range" = IQR(x, na.rm = TRUE)
  )
}

# Calculate the statistics for males (m)
summary_m <- data %>%
  filter(sex == "m") %>%
  summarise(across(where(is.numeric), calc_stats))

# Set row names for the summary table of males
rownames(summary_m) <- c("Average", "Median", "Minimum", "Maximum", "Std. Deviation",
                         "Variance", "Lower Quartile (25%)", "Upper Quartile (75%)", "Interquartile Range")

# Calculate the statistics for females (f)
summary_f <- data %>%
  filter(sex == "f") %>%
  summarise(across(where(is.numeric), calc_stats))

# Set row names for the summary table of females
rownames(summary_f) <- c("Average", "Median", "Minimum", "Maximum", "Std. Deviation",
                         "Variance", "Lower Quartile (25%)", "Upper Quartile (75%)", "Interquartile Range")

# Display the results for males (m)
cat("Summary for males (m):\n")
summary_mdf = t(format(summary_m, scientific = FALSE))

# Display the results for females (f)
cat("\nSummary for females (f):\n")
summary_fdf = t(format(summary_f, scientific = FALSE))

# Export both tables
write.csv(summary_mdf,paste0(dirname(loaddata),"/featurestatisticsmale.csv"))
write.csv(summary_fdf,paste0(dirname(loaddata),"/featurestatisticsfemale.csv"))

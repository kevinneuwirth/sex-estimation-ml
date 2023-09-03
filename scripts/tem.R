### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Prompt the user to select the folder with repeated measurements
repeated_measurements_folder <- choose.dir()

# Prompt the user to select the folder for comparison
comparison_folder <- choose.dir()

read_fcsv <- function(file_path) {
  # Read the file, skipping the header lines
  data <- read.csv(file_path, skip = 3, header = FALSE, sep = ",", na.strings = "NA")
  
  # Select specific columns: v2, v3, v4, and v12
  data <- data[, c(2, 3, 4, 12)]
  
  # Set column names for the selected columns
  colnames(data) <- c("x", "y", "z", "label")
  
  return(data)
}

# List the files in both folders
repeated_files <- list.files(repeated_measurements_folder, pattern = ".fcsv", full.names = TRUE)
comparison_files <- list.files(comparison_folder, pattern = ".fcsv", full.names = TRUE)

# Create an empty list to store TEM results for each landmark
tem_results <- list()

# Iterate through the repeated measurement files
for (repeated_file in repeated_files) {
  # Extract the filename (without path) for matching
  repeated_filename <- tools::file_path_sans_ext(basename(repeated_file))
  # Extract the first three characters from the filename
  repeated_filename_prefix <- substr(repeated_filename, 1, 3)
  
  # Find the corresponding file in the comparison folder by matching the first three characters
  matching_comparison_files <- list.files(comparison_folder, pattern = paste0("^", repeated_filename_prefix), full.names = TRUE)
  
  # Check if any matching files exist
  if (length(matching_comparison_files) > 0) {
    # Load the repeated measurements and comparison data using the read.csv function with the correct separator
    repeated_data <- read.csv(repeated_file, skip = 3, sep = ",", header = FALSE)
    comparison_data <- read.csv(matching_comparison_files[1], skip = 3, sep = ",", header = FALSE) # Assuming you want to use the first matching file
    
    # Assuming the data structures are identical, calculate TEM for each landmark
    tem <- sqrt(rowMeans((repeated_data[, c(2, 3, 4)] - comparison_data[, c(2, 3, 4)])^2))
    
    # Store TEM results in the list, indexed by landmark name
    landmark_names <- repeated_data[, 12]  # Assuming the landmark label is in column 12
    for (i in 1:length(landmark_names)) {
      landmark_name <- landmark_names[i]
      if (!is.na(landmark_name)) {
        if (!exists(landmark_name, where = tem_results)) {
          tem_results[[landmark_name]] <- numeric(0)
        }
        tem_results[[landmark_name]] <- c(tem_results[[landmark_name]], tem[i])
      }
    }
  } else {
    cat("Matching file not found for", repeated_filename, "\n")
  }
}

# Calculate the average TEM for each landmark
tem_per_landmark <- sapply(tem_results, function(tem_list) mean(tem_list, na.rm = TRUE))

# Calculate the median TEM for each landmark
median_tem_per_landmark <- sapply(tem_results, function(tem_list) median(tem_list, na.rm = TRUE))

# Calculate the minimum TEM for each landmark
min_tem_per_landmark <- sapply(tem_results, function(tem_list) min(tem_list, na.rm = TRUE))

# Calculate the maximum TEM for each landmark
max_tem_per_landmark <- sapply(tem_results, function(tem_list) max(tem_list, na.rm = TRUE))

# Calculate the standard deviation of TEM for each landmark
sd_tem_per_landmark <- sapply(tem_results, function(tem_list) sd(tem_list, na.rm = TRUE))

# Create a data frame containing the results
result_table <- data.frame(
  Landmark = names(tem_per_landmark),
  Mean = tem_per_landmark,
  Median = median_tem_per_landmark,
  SD = sd_tem_per_landmark,
  Min = min_tem_per_landmark,
  Max = max_tem_per_landmark
)

# Print the table
print("Summary of TEM per landmark:")
print(result_table)

# Export the summary table to a CSV file in the original folder
output_csv_file <- file.path(repeated_measurements_folder, "TEM_Summary.csv")
write.csv(result_table, file = output_csv_file, row.names = FALSE)

######## plot generation ########
# Create a histogram for the mean TEM values
hist(tem_per_landmark, main = "Distribution of Mean TEMs for all Landmarks",
     xlab = "Mean TEM Value", ylab = "Frequency", col = "lightblue", border = "black", breaks = 20)

### plot for all landmarks ### 
# Create a list to store TEM values for all landmarks
all_tem_values <- list()

# Loop through each landmark
for (landmark_name in names(tem_results)) {
  # Get TEM values for the current landmark
  tem_values <- tem_results[[landmark_name]]
  all_tem_values[[landmark_name]] <- tem_values
}

# Create a boxplot to show the TEM distribution for all landmarks
boxplot(all_tem_values, names = names(tem_results), 
        main = "TEM Distribution for All Landmarks",
        xlab = "Landmark", ylab = "TEM Value", col = "lightblue")

### the same, but ordered based on mean TEMs ###
# Create a list to store TEM values for all landmarks
all_tem_values <- list()

# Loop through each landmark
for (landmark_name in names(tem_results)) {
  # Get TEM values for the current landmark
  tem_values <- tem_results[[landmark_name]]
  all_tem_values[[landmark_name]] <- tem_values
}

# Calculate the mean TEM values for each landmark
mean_tem_values <- sapply(all_tem_values, function(tem_list) mean(tem_list, na.rm = TRUE))

# Sort the landmarks based on mean TEM values
sorted_landmarks <- names(mean_tem_values)[order(mean_tem_values)]

# Create a boxplot to show the TEM distribution for all landmarks, sorted by mean TEM
boxplot(all_tem_values[sorted_landmarks], names = sorted_landmarks, 
        main = "TEM Distribution for All Landmarks (Sorted by Mean TEM)",
        xlab = "Landmark", ylab = "TEM Value", col = "lightblue")


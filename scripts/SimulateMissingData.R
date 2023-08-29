### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Load the required libraries
library(dplyr)

# Choose and read the CSV data
file_path <- choose.files(default = "Select dataset", caption = "Select dataset", multi = FALSE)
data <- read.csv(file_path, check.names = FALSE)

# Function to randomly remove values from a vector
randomly_remove_values <- function(x, percentage_to_remove) {
  n <- length(x)
  num_values_to_remove <- round(n * percentage_to_remove / 100)
  indices_to_remove <- sample(1:n, num_values_to_remove)
  x[indices_to_remove] <- NA
  return(x)
}

# Function to apply randomly_remove_values to all columns except "sex"
randomly_remove_data <- function(data, percentage_to_remove) {
  data %>%
    mutate(across(-sex, ~ randomly_remove_values(., percentage_to_remove)))
}

# Set the percentage of data to remove (adjust this variable as needed)
percentage_to_remove <- 5

# Apply randomly_remove_data function
result <- as.data.frame(randomly_remove_data(data, percentage_to_remove))

# Export as .csv
prunedBasename <- paste0(gsub(pattern="\\.csv$","",basename(file_path)))
output_file <- file.path(dirname(file_path), paste0(prunedBasename,"missing",percentage_to_remove,"percent.csv"))
write.csv(result,file=output_file,row.names=FALSE)

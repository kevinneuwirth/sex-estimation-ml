### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Read the importances CSV file
datafile <- choose.files(default = "Select importancesheet", caption = "Select importancesheet", multi = FALSE)

#define number of top predictors ("rows") to be grabbed
toppred <- 3

data <- read.csv(datafile, nrows = toppred,check.names = FALSE)

# Extract the variable column and split the components
variable_components <- strsplit(data$variable, ":")

# Initialize counters for component and underscored component proportions
component_proportions <- list()

# Iterate over the rows
for (i in 1:toppred) {
  # Get the components for the current row
  components <- unlist(variable_components[i])
  
  # Remove the attachment introduced with "_"
  components <- gsub("_.*", "", components)
  
  # Calculate the proportion of each component
  for (comp in components) {
    if (is.null(component_proportions[[comp]])) {
      component_proportions[[comp]] <- 1
    } else {
      component_proportions[[comp]] <- component_proportions[[comp]] + 1
    }
  }
}

# Calculate the proportion of each component
component_proportions <- lapply(component_proportions, function(count) count / toppred)

# Print the proportions of the components
print(component_proportions)

# Initialize counters for specific component proportions
angle_count <- 0
area_count <- 0
distance_count <- 0

# Iterate over the rows
for (i in 1:toppred) {
  # Get the components for the current row
  components <- unlist(variable_components[i])
  
  # Check for specific components and increment the respective counter
  if (any(grepl("_angle", components))) {
    angle_count <- angle_count + 1
  }
  if (any(grepl("_area", components))) {
    area_count <- area_count + 1
  }
  if (any(grepl("_distance", components))) {
    distance_count <- distance_count + 1
  }
}

# Calculate the proportion of each specific component
angle_proportion <- angle_count / toppred
area_proportion <- area_count / toppred
distance_proportion <- distance_count / toppred

# Print the proportions of specific components
print(angle_proportion)
print(area_proportion)
print(distance_proportion)

# Create a data frame for the proportions
proportions <- data.frame(component = c("_angle", "_area", "_distance"),
                          proportion = c(angle_proportion, area_proportion, distance_proportion))

# Write the proportions to a CSV file
output_file1 <- paste0(dirname(datafile), "/type_proportions",toppred,".csv")
write.csv(proportions, file = output_file1, row.names = FALSE)

# Write the component proportions to a CSV file in the same path as the loaded CSV file
output_file2 <- paste0(dirname(datafile), "/component_proportions",toppred,".csv")
write.csv(as.data.frame(component_proportions), file = output_file2, row.names = FALSE)

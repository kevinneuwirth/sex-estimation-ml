### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Load required libraries
library(dplyr)

# Create a sample dataset 1
dataset1f <- choose.files(default = "Select importancesheet", caption = "Select importancesheet", multi = FALSE)
dataset2f <- choose.files(default = "Select database", caption = "Select database", multi = FALSE)

# Load the data sets. nrows= defines the amount of top features to load; nrows = 3 loads up the top 3 features for example
dataset1 <- read.csv(dataset1f, nrows = 3)
dataset2 <- read.csv(dataset2f, check.names = FALSE)

# Extract the variable names from dataset 1
variableNames <-  c("sex",dataset1[["variable"]])

# Prune dataset 2 based on variableNames using select()
prunedDataset2 <- dataset2 %>% select(all_of(variableNames))

# Write the proportions to a CSV file
output_file1 <- paste0(dirname(dataset2f), "/",gsub(pattern="\\.csv$","",basename(dataset2f)),"prunedtop",nrow(dataset1),".csv")
write.csv(prunedDataset2, file = output_file1, row.names = FALSE)


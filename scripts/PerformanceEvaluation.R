### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Load the required libraries
library(h2o)

# Start the H2O cluster
h2o.init()

# Load the test data
test_data_path <- choose.files(default = "Select test dataset", caption = "Select test dataset", multi = FALSE)
test_data <- h2o.importFile(test_data_path)

# Load the saved H2O machine learning model
model_path <- choose.files(default = "Select model", caption = "Select model", multi = FALSE)
saved_model <- h2o.loadModel(model_path)

# Use the model to make predictions on the test data
predictions <- h2o.predict(saved_model, test_data)

# Calculate the performance of the model using h2o.performance()
performance <- h2o.performance(saved_model, test_data)
# ... or without test data, if you just want to have a look at the model metrics
#performance <- h2o.performance(saved_model)

# Print the performance metrics
print(performance)

# Generate a residual analysis plot
#h2o.residual_analysis_plot(saved_model)

# Export the performance as a text file to the folder above the model folder
output_folder <- file.path(dirname(dirname(model_path)))
prunedBasename <- paste0(gsub(pattern="\\.csv$","",basename(test_data_path)))
output_file <- paste0(output_folder, "/performance",prunedBasename,".txt")
capture.output(performance, file = output_file)

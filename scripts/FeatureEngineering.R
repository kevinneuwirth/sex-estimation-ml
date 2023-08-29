### Note: ChatGPT 3.5 was used as a co-pilot to write this script

# Install and load the h2o package
if (!require("h2o")) {
  install.packages("h2o")
}
library(h2o)

# Start an H2O cluster
h2o.init(max_mem_size = "12G")

# Select and load your dataset into H2O
datafile <- choose.files(default = "Select database for feature engineering", caption = "Select database", multi = FALSE)
database <- h2o.importFile(datafile)

# Set the predictors and response;
x = names(database)[3:ncol(database)]
y <- "sex"

# For binary classification, response should be a factor
database[, y] <- as.factor(database[, y])

# Set the Lasso alpha parameter (0 for L1 regularization)
alpha <- 1

# Run GLM Lasso with cross-validation to determine the optimal lambda value
lambda_search <- h2o.glm(
  x = x,
  y = y,
  training_frame = database,
  nfolds = 5,
  alpha = alpha,
  lambda_search = TRUE,
  max_iterations = 1000
)

# Get the lambda value with the best performance
best_lambda <- lambda_search@model$lambda_best

# Run GLM Lasso with cross-validation and the selected lambda value
glm_model <- h2o.glm(
  x = x,
  y = y,
  training_frame = database,
  nfolds = 5,
  alpha = alpha,
  lambda = best_lambda,
  max_iterations = 10000
)

# Get variable importances
var_importances <- h2o.varimp(glm_model)

# Sort the variable importances by decreasing order
sorted_var_importances <- var_importances[order(var_importances$relative_importance, decreasing = TRUE), ]

# Select the top 50 predictor variables
selected_predictors <- sorted_var_importances$variable[1:50]

# Print the selected predictor variables
print(selected_predictors)
write.csv(var_importances,"importances.csv")



# Get the coefficients of the model
coefficients <- as.data.frame(h2o.coef(glm_model))

# Sort the coefficients by their absolute values
sorted_coeffs <- coefficients[order(abs(coefficients$standardized_coefficients), decreasing = TRUE), ]

# Select the top 50 predictor variables based on the absolute coefficient values
selected_predictors <- row.names(sorted_coeffs)[1:50]

# Print the selected predictor variables
print(selected_predictors)
write.csv(coefficients,"coefficients.csv")

h2o.saveModel(object=glm_model,path=getwd())

# Stop the H2O cluster
h2o.shutdown()

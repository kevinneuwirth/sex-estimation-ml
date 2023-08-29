library(h2o)

# Connect to server:
#h2o.connect(ip = "localhost", port = 54321)

# Start the H2O cluster (locally or on the server)
h2o.init(max_mem_size="12G")

# Set up working directory (important for output)
workingdir <- choose.dir(default = "Select output directory", caption = "Select output directory")
setwd(workingdir)
# Choose training file
trainingdata <- choose.files(default = "Select training data", caption = "Select training data", multi = FALSE)
# Choose validation file
#validationdata <- choose.files(default = "Select validation data", caption = "Select validation data", multi = FALSE)
# Import training database from local disk:
traindata <- h2o.importFile(trainingdata)
# Split database into training and validation_frame:
traindata_split <- h2o.splitFrame(data = traindata, ratios = 0.9, seed = 123)
train <- traindata_split[[1]]
test <- traindata_split[[2]]

# Identify predictors and response
y <- "sex"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[, y] <- as.factor(train[, y])
test[, y] <- as.factor(test[, y])

# Run AutoML for 50 base models
aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  nfold = 10,
                  max_models = 50,
                  seed = 0)

# Get leaderboard with all possible columns
lb <- h2o.get_leaderboard(object = aml, extra_columns = "ALL")
print(lb, n = nrow(lb))

# Get the best model using the metric
m <- aml@leader
m2 <- h2o.get_best_model(aml,algorithm = "stackedensemble")
# Get the best model using a non-default metric
#m <- h2o.get_best_model(aml, criterion = "logloss")

# Get the best XGBoost model using default sort metric
#xgb <- h2o.get_best_model(aml, algorithm = "xgboost")

# Get the best XGBoost model, ranked by logloss
#xgb <- h2o.get_best_model(aml, algorithm = "xgboost", criterion = "logloss")

# View the non-default parameter values for the XGBoost model above
#xgb@parameters

# Get AutoML event log
log <- aml@event_log

# Get training timing info
info <- aml@training_info

# Get Learning Curves only
h2o.learning_curve_plot(m)
h2o.learning_curve_plot(m2)

# Explain a model
exm <- h2o.explain(m, test)
exm

exm2 <- h2o.explain(m2, test)
exm2

# Explain an AutoML object
exa <- h2o.explain(aml, test)
exa

# Predict (using the best model)
pred = h2o.predict(object = m, newdata = test)
summary(pred$p1, exact_quantiles = TRUE)

# Retrieve model performance
perf <- h2o.performance(m, test)
perf
perf <- h2o.performance(m2, test)
perf
accuracy = h2o.accuracy(perf)

# Export all models
currenttime <- format(Sys.time(),"%d-%m-%Y_%H-%M-%S")
getpath <- getwd()
savepath <- file.path(getpath,currenttime,"amlmodels")
mod_ids <- aml@leaderboard$model_id
for(i in 1:nrow(mod_ids)) {
  
  aml1 <- h2o.getModel(aml@leaderboard[i, 1]) # get model object in environment
  h2o.saveModel(object = aml1,path=savepath,force=TRUE) # pass that model object to h2o.saveModel as an argument
  
}
# Export leaderboard, log etc.
savepath2 <- file.path(getpath,currenttime,"leaderboard.csv")
lbdf <- as.data.frame(lb)
write.csv(lbdf,file=savepath2)

logdf <- as.data.frame(log)
savepath3 <- file.path(getpath,currenttime,"log.csv")
write.csv(logdf,file=savepath3)

infodf <- as.data.frame(info)
savepath4 <- file.path(getpath,currenttime,"info.csv")
write.csv(infodf,file=savepath4)

savepath5 <- file.path(getpath,currenttime,"varimpheat.png")
png(file=savepath5,width=600,height=500)
plot(h2o.varimp_heatmap(aml))
dev.off()

savepath6 <- file.path(getpath,currenttime,"learningcurve.png")
png(file=savepath6,width=700,height=500)
plot(h2o.learning_curve_plot(m))
dev.off()

savepath7 <- file.path(getpath,currenttime,"learningcurvestackedensemble.png")
png(file=savepath7,width=700,height=500)
plot(h2o.learning_curve_plot(m2))
dev.off()

accuracydf <- as.data.frame(accuracy)
savepath8 <- file.path(getpath,currenttime,"accuracy.csv")
write.csv(accuracydf,file=savepath8)

savepath9 <- file.path(getpath,currenttime,"train.csv")
write.csv(as.data.frame(train,check.names=F),file=savepath9,check.names=FALSE)

savepath10 <- file.path(getpath,currenttime,"test.csv")
write.csv(as.data.frame(test,check.names=F),file=savepath10,check.names=FALSE)

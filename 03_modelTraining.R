# Modelling Phase --------------------------------------------------------------

# load data (preprocessed and cleaned)
train <- readRDS('/data/trainPreprocessed.RData')
test <- readRDS('/data/testPreprocessed.RData')

# Some treatments to variables to make sure that H20 will not encounter any issues
# related to variables class ----------------

# select columns to be used
train <- train[, c(4,13:18,21:110)]
test <- test[, c(4,13:18,21:110)]
# convert categorical variables to factors
train[, 2:6] <- lapply(train[,2:6], factor)
test[, 2:6] <- lapply(test[,2:6], factor)
# convert binary variables to logical
train[, 15:ncol(train)] <- lapply(train[,15:ncol(train)], 
                                 function(x) as.logical(x))
test[, 15:ncol(train)] <- lapply(test[,15:ncol(train)], 
                                  function(x) as.logical(x))

# Initialize h2o instance
localH2O = h2o.init(nthreads = -1)
# Map to H20 data frame type - change training data to preferred columns
train.h2o  <- as.h2o(train, destination_frame = "Training")
test.h2o   <- as.h2o(test, destination_frame = "Test")
# Define independent and dependent vars
y <- 1
x <- 2:97

# set seed
seed = set.seed(1234)

################################# Base - random forest  ########################################
rforest.model <- h2o.randomForest(y=y, x=x,
                                  training_frame = train.h2o,
                                  ntrees = 1000,
                                  mtries = 3,
                                  max_depth = 4,
                                  seed = seed,
                                  nfolds=5,
                                  stopping_metric = "AUC")

# return AUC
rforest.model@model$training_metrics@metrics$AUC
##############################################################################################

############################### Grid Search - random forest  #################################
# Grid search parameters
ntrees <- c(1000,2000)
mtries <- c(5,10,20)
max_depth <- c(20, 10)
hyper_params_rf <- list(ntrees=ntrees, mtries=mtries, max_depth=max_depth)

# Grid search - random forest
model_grid_rf <- h2o.grid("randomForest",
                          hyper_params = hyper_params_rf,
                          nfolds=5,
                          x = x, 
                          y = y,
                          training_frame = train.h2o,
                          seed = 1234, # 1234, seed
                          stopping_metric = "AUC")

# Create empty df to save auc
aucs_rf <- data.frame(model_id=character(0), 
                      auc=numeric(0), stringsAsFactors = F)

# print out the Test AUC for all of the models (and save in df)
for (model_id in model_grid_rf@model_ids) {
  model <- h2o.getModel(model_id)
  auc <- h2o.auc(model, xval = TRUE)
  aucs_rf <- rbind(aucs_rf, data.frame(model_id=model_id, auc=auc, stringsAsFactors = F))
  print(sprintf("Test set auc: %f", auc))
}

# find model with best auc
best_model_rf <- aucs_rf$model_id[which.max(aucs_rf$auc)]
best_model_rf <- h2o.getModel(best_model_rf)

# save model
h2o.saveModel(object=best_model_rf, path=paste0(getwd(),"/models/"), force=TRUE)

# variable importance
h2o.varimp(best_model_rf)

# confusion matrix
h2o.confusionMatrix(best_model_rf)

# predict random
predicted_labels_rf <- as.data.frame(h2o.predict(best_model,_rd test.h2o))

# TO-DO: retrieve labels and format to required format
##############################################################################################

####################################### Base - gbm  ##########################################
gbm.model <- h2o.gbm(y=y, x=x, 
                     training_frame = train.h2o, 
                     seed = seed,
                     ntrees = 1000, 
                     max_depth = 4, 
                     learn_rate = 0.1,
                     nfolds=5, 
                     stopping_metric = "AUC")

# return AUC
gbm.model@model$training_metrics@metrics$AUC
##############################################################################################

#################################### Grid Search - gbm  ######################################
# grid search parameters
ntrees_gbm <- c(500,1000)
max_depth_gbm <- c(5,20)
learn_rate_gbm <- c(0.03, 0.1, 0.3)
hyper_params_gbm <- list(ntrees=ntrees_gbm, max_depth=max_depth_gbm, learn_rate=learn_rate_gbm)

# grid search - random forest
model_grid_gbm <- h2o.grid("gbm",
                           hyper_params = hyper_params_gbm,
                           nfolds=2,
                           x = x, 
                           y = y,
                           training_frame = train.h2o,
                           seed = seed, # 1234
                           stopping_metric = "AUC")

# print out the Test AUC for all of the models (and save in df)
aucs_gbm <- data.frame(model_id=character(0), auc=numeric(0), stringsAsFactors = F)
for (model_id in model_grid_gbm@model_ids) {
  model <- h2o.getModel(model_id)
  auc <- h2o.auc(model, xval = TRUE)
  aucs_gbm <- rbind(aucs_gbm, data.frame(model_id=model_id, auc=auc, stringsAsFactors = F))
  print(sprintf("Test set auc: %f", auc))
}

# find model with best auc
best_model_gbm <- aucs_gbm$model_id[which.max(aucs_gbm$auc)]
best_model_gbm <- h2o.getModel(best_model_gbm)

# variable importance
h2o.varimp(best_model_gbm)

# confusion matrix
h2o.confusionMatrix(best_model_gbm)

# predict random
predicted_labels_gbm <- as.data.frame(h2o.predict(best_model_gbm, test.h2o))

# TODO retrieve labels and format to required format
 

source('00_initialization.R')
source('02_preProcessing.R')
source('03_modelTraining.R')

# ----- Execution parameters ---------------------------------------------------
# Features that can be added to the Dataset
addZipCode <- TRUE
useSegimon <- FALSE
addLangVars <- TRUE
addGeoVars <- TRUE
useDummyVars <- TRUE
addPublicPlaceVar <- TRUE
addFamilyReviewVar <- TRUE
doTraining <- TRUE
doInference <- FALSE
modelChoice <- "GBM_greedy" # RF, RF_greedy, GBM, GBM_greedy
# grid search parameters of Random Forest and Gradient Boosting
paramsModel <- list(ntrees_rf =  c(1000,2000),
                    mtries_rf =  c(5,10,20),
                    max_depth_rf = c(20, 10),
                    ntrees_gbm = c(500,1000),
                    max_depth_gbm = c(5,20),
                    learn_rate_gbm = c(0.03, 0.1, 0.3),
                    nfolds = 5)

# ----- Data Import ------------------------------------------------------------
cat("Reading the train and test data \n")
train  <- fread('data/train.csv')
test   <- fread('data/test.csv')
segimon <- fread('data/taula_segimon.csv')

# Exploratory Data Analysis ----------------------------------------------------
exploratoryDataAnalysis(train)

# Data Processing and Feature Engineering --------------------------------------
trainPro <- performDataProcessing(dt = copy(train), 
                                  set = "train",
                                  segimon = segimon,
                                  addZipCode = addZipCode,
                                  useSegimon = useSegimon,
                                  addLangVars = addLangVars,
                                  addGeoVars = addGeoVars,
                                  useDummyVars = useDummyVars,
                                  addPublicPlaceVar = addPublicPlaceVar, 
                                  addFamilyReviewVar = addFamilyReviewVar)

testPro <- performDataProcessing(dt = copy(test),
                                 set = "test",
                                 segimon = segimon,
                                 addZipCode = addZipCode,
                                 useSegimon = useSegimon,
                                 addLangVars = addLangVars,
                                 addGeoVars = addGeoVars,
                                 useDummyVars = useDummyVars,
                                 addPublicPlaceVar = addPublicPlaceVar,
                                 addFamilyReviewVar = addFamilyReviewVar)

# Pre-modelling phase ---------------------------------------------------------- 
dtModeling <- preModelingChecks(train = trainPro, 
                                test = testPro)
trainModeling <- dtModeling$train
testModeling <- dtModeling$test

# Saving Training and test set so these can be picked by someone else in the team
saveRDS(trainModeling, '/data/trainModeling.RData')
saveRDS(testModeling, '/data/testModeling.RData')

# The training is done in the script 03_modelTraining
doTrainAndInference(doTraining = doTraining, 
                    doInference = doInference, 
                    paramsModel = paramsModel, 
                    modelChoice = modelChoice)
